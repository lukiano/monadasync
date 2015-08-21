package io.atlassian.monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService, TimeUnit }

import cats.data.{ Kleisli, ReaderT, XorT, Xor }
import cats.state.StateT

import scala.concurrent.duration.Duration
import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._

/**
 * Provides asynchronous operations for F
 */
trait MonadAsync[F[_]] {

  /**
   * @return an F whose value is immediately set.
   */
  def now[A](a: A): F[A] =
    Monad[F].pure(a)

  /**
   * @return an F whose value will be set from an asynchronous computation, via callback.
   */
  def async[A](listen: Callback[A]): F[A]

  /**
   * @return an F whose value will be computed when called, on the caller thread.
   */
  def delay[A](a: => A): F[A]

  /**
   * @return an F[A] wrapped in a suspension to be computed when called, on the caller thread.
   */
  def suspend[A](fa: => F[A]): F[A] =
    now(()) >> fa

  /**
   * @return an F whose value will be computed when called, in a thread within the given pool.
   */
  def async[A](a: => A)(implicit pool: Executor): F[A] =
    async { cb =>
      pool.execute { new Runnable { def run() = cb(a) } }
    }

  /**
   * Asynchronous bind. f executes in a thread within the given pool.
   */
  def bindA[A, B](fa: F[A])(f: A => F[B])(implicit pool: Executor): F[B] =
    mapA(fa)(f) >>= identity

  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork[A](fa: => F[A])(implicit pool: Executor): F[A] =
    async(fa) >>= identity

  /**
   * Asynchronous map. f executes in a thread within the given pool.
   */
  def mapA[A, B](fa: F[A])(f: A => B)(implicit pool: Executor): F[B] =
    fa >>= { a => async(f(a)) }

  /**
   * @return an F[A] whose value will be set after a given delay, and future operation will execute in a thread within the given pool.
   */
  def schedule[A](a: => A, delay: Duration)(implicit pool: ScheduledExecutorService): F[A] =
    async { cb =>
      pool.schedule(new Runnable {
        def run() = cb(a)
      }, delay.toMillis, TimeUnit.MILLISECONDS)
      ()
    }

  /**
   * @return the underlying monad.
   */
  def monad: Monad[F] = M

  protected implicit def M: Monad[F]

  val monadAsyncSyntax = new MonadAsync.MonadAsyncSyntax[F] {}
}

/**
 * Some laws any MonadAsync implementation should obey.
 */
trait MonadAsyncLaws[F[_]] {
  implicit def F: MonadAsync[F]
  implicit val Monad: Monad[F] = F.monad
  import MonadAsync.syntax._
  import cats.laws._
  def asyncIsDelay[A](a: () => A): IsEq[F[A]] =
    F.delay(a()) <-> F.async(a())(MonadAsync.SameThreadExecutor)
  def bindAIsBind[A, B](a: A, f: A => F[A], b: A => F[B]): IsEq[F[B]] =
    f(a).flatMap(b) <-> f(a).flatMapA(b)(MonadAsync.SameThreadExecutor)
  def mapAIsMap[A, B](a: A, f: A => F[A], b: A => B): IsEq[F[B]] =
    f(a).map(b) <-> f(a).mapA(b)(MonadAsync.SameThreadExecutor)
}
object MonadAsyncLaws {
  def apply[F[_]](implicit ev: MonadAsync[F]): MonadAsyncLaws[F] =
    new MonadAsyncLaws[F] { override def F: MonadAsync[F] = ev }
}

object MonadAsync extends MonadAsyncInstances {
  def apply[F[_]: MonadAsync] = implicitly[MonadAsync[F]]

  trait MonadAsyncSyntax[F[_]] {
    implicit def ToMonadAsyncOps[A](v: F[A])(implicit F0: MonadAsync[F]): MonadAsyncOps[F, A] =
      new MonadAsyncOps[F, A](v)(F0)
  }

  /**
   * Used to test laws.
   */
  object SameThreadExecutor extends Executor {
    def execute(command: Runnable) = command.run()
  }

  object syntax extends MonadAsyncFunctions {
    implicit def ToMonadAsyncOps[F[_], A](v: F[A])(implicit MA: MonadAsync[F]): MonadAsyncOps[F, A] =
      new MonadAsyncOps[F, A](v)

    /**
     * Callback for operations that may return errors.
     */
    implicit class EitherAsync[L, A](val cb: Callback[L Xor A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], ME: MonadError[({ type l[α, β] = F[β] })#l, L]): F[A] =
        MA.async(cb) >>= {
          case Xor.Right(a) => ME.pure(a)
          case Xor.Left(l)  => ME.raiseError(l)
        }
    }

    /**
     * Callback => F.
     */
    implicit class CallbackAsync[A](val cb: Callback[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F]): F[A] =
        MA.async(cb)
    }

    /**
     * Future ~> F
     */
    implicit class FutureAsync[A](val f: Future[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F]): F[A] =
        f.callback.liftAsync[F]
    }
    implicit def FutureTransformation[F[_]](implicit MA: MonadAsync[F]): Future ~> F =
      new (Future ~> F) {
        def apply[A](f: Future[A]): F[A] = f.liftAsync[F]
      }

    /**
     * Any to F.
     */
    implicit class AnyAsync[A](val a: A) extends AnyVal {
      def now[F[_]](implicit MA: MonadAsync[F]): F[A] =
        MA.now(a)
    }
  }
}

trait MonadAsyncFunctions {
  def schedule[F[_], A](delay: Duration)(a: => A)(implicit MA: MonadAsync[F], pool: ScheduledExecutorService): F[A] =
    MA.schedule(a, delay)

  def suspend[F[_], A](fa: => F[A])(implicit MA: MonadAsync[F]): F[A] =
    MA.suspend(fa)
}

trait MonadAsyncInstances {
  import MonadAsync.syntax._

  implicit val FutureMonadAsync = new MonadAsync[Future] {
    protected def M = Monad[Future]

    def async[A](listen: Callback[A]) =
      Future.async(listen)
    def delay[A](a: => A) =
      Future.delay(a)
  }

  implicit def XorTMonadAsync[F[_]: MonadAsync, L] = new MonadAsync[XorT[F, L, ?]] {
    private implicit val MF: Monad[F] = MonadAsync[F].monad

    def delay[A](a: => A): XorT[F, L, A] =
      XorT.right[F, L, A](MonadAsync[F].delay(a))
    def async[A](listen: Callback[A]): XorT[F, L, A] =
      XorT.right[F, L, A](listen.liftAsync[F])

    protected def M =
      Monad[XorT[F, L, ?]]
  }

  implicit def ReaderTMonadAsync[F[_]: MonadAsync, E] = new MonadAsync[ReaderT[F, E, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def delay[A](a: => A): ReaderT[F, E, A] =
      kleisli { MonadAsync[F].delay(a) }
    def async[A](listen: Callback[A]): ReaderT[F, E, A] =
      kleisli { listen.liftAsync[F] }

    protected def M =
      Monad[ReaderT[F, E, ?]]
    private def kleisli[A](fa: => F[A]): ReaderT[F, E, A] =
      Kleisli { _ => fa }
  }

  implicit def StateTMonadAsync[F[_]: MonadAsync, S] = new MonadAsync[StateT[F, S, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def delay[A](a: => A): StateT[F, S, A] =
      state { MonadAsync[F].delay(a) }
    def async[A](listen: Callback[A]): StateT[F, S, A] =
      state { listen.liftAsync[F] }

    protected def M =
      Monad[StateT[F, S, ?]]
    private def state[A](fa: => F[A]): StateT[F, S, A] =
      StateT { s => fa map { s -> _ } }
  }
}

final class MonadAsyncOps[F[_], A](self: F[A])(implicit MA: MonadAsync[F]) {
  /**
   * Asynchronous bind
   */
  def flatMapA[B](f: A => F[B])(implicit pool: Executor): F[B] =
    MA.bindA(self)(f)

  /**
   * Asynchronous map
   */
  def mapA[B](f: A => B)(implicit pool: Executor): F[B] =
    MA.mapA(self)(f)

  /**
   * Continue running after d.
   */
  def after(d: Duration): F[A] =
    after(d.toMillis)

  /**
   * Continue running after t milliseconds.
   */
  def after(t: Long): F[A] = {
    implicit val M = MA.monad
    Timer.default.valueWait[F, Unit]((), t) >> self
  }

  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork(implicit pool: Executor): F[A] =
    MA.fork(self)
}