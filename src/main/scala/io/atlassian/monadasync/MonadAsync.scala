package io.atlassian.monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService, TimeUnit }

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent.{ Future, Task }
import scalaz.syntax.monad._

/**
 * Provides asynchronous operations for F
 */
trait MonadAsync[F[_]] {

  /**
   * @return an F whose value is immediately set.
   */
  def now[A](a: A): F[A]

  /**
   * @return an F whose value will be set from an asynchronous computation, via callback.
   */
  def async[A](listen: Callback[A]): F[A]

  /**
   * @return an F whose value will be computed when called, on the caller thread.
   */
  def delay[A](a: => A): F[A] =
    Monad[F].point(a)

  /**
   * @return an F[A] wrapped in a suspension to be computed when called, on the caller thread.
   */
  def suspend[A](fa: => F[A]): F[A] =
    delay(()) >> fa

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

  /**
   * Some laws any MonadAsync implementation should obey.
   * TODO include nondeterminism
   */
  trait MonadAsyncLaw {
    def asyncIsDelay[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(a()), async(a())(MonadAsync.SameThreadExecutor))
    def bindAIsBind[A, B](a: A, f: A => F[A], b: A => F[B])(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) >>= b, bindA(f(a))(b)(MonadAsync.SameThreadExecutor))
    def mapAIsMap[A, B](a: A, f: A => F[A], b: A => B)(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) map b, mapA(f(a))(b)(MonadAsync.SameThreadExecutor))
  }
  def monadAsyncLaw = new MonadAsyncLaw {}
}

object MonadAsync extends MonadAsyncFunctions with MonadAsyncInstances {
  def apply[F[_]: MonadAsync] = implicitly[MonadAsync[F]]

  trait MonadAsyncSyntax[F[_]] {
    implicit def ToMonadAsyncOps[A](v: F[A])(implicit F0: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)(F0)
  }

  /**
   * Used to test laws.
   */
  object SameThreadExecutor extends Executor {
    def execute(command: Runnable) = command.run()
  }

  object syntax {
    implicit def ToMonadAsyncOps[F[_], A](v: F[A])(implicit MA: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)

    /**
     * Callback for operations that may return errors.
     */
    implicit class EitherAsync[L, A](val cb: Callback[L \/ A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], ME: MonadError[({ type l[α, β] = F[β] })#l, L]): F[A] =
        MA.async(cb) >>= {
          case \/-(a) => a.point[F]
          case -\/(l) => ME.raiseError(l)
        }
    }

    /**
     * Callback for operations that may contain logs.
     */
    implicit class WriterAsync[W, A](val cb: Callback[(W, A)]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MT: MonadTell[({ type l[α, β] = F[β] })#l, W]): F[A] =
        MA.async(cb) >>= {
          case (w, a) => MT.writer(w, a)
        }
    }

    /**
     * Callback for operations depend on a configuration.
     */
    implicit class ReaderAsync[R, A](val f: R => Callback[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MR: MonadReader[({ type l[α, β] = F[β] })#l, R]): F[A] =
        MR.ask >>= { r =>
          MA.async(f(r))
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
     * Task ~> F.
     */
    implicit class TryAsync[A](val t: Task[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): F[A] = {
        implicit val M = MA.monad
        MA.async(t.callback).unattempt
      }
    }
    implicit def TaskTransformation[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): Task ~> F =
      new (Task ~> F) {
        def apply[A](t: Task[A]): F[A] = t.liftAsync[F]
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

  def tryCatch[F[_]: Monad: Catchable, A](a: => A): F[A] = {
    Task.Try(a).point[F].unattempt
  }

  def asyncCatch[F[_], A](a: => A)(implicit MA: MonadAsync[F], C: Catchable[F], pool: Executor): F[A] = {
    implicit val M = MA.monad
    MA.async(Task.Try(a)).unattempt
  }
}

trait MonadAsyncInstances {
  import MonadAsync.syntax._

  implicit val FutureMonadAsync = new MonadAsync[Future] {
    protected def M = Monad[Future]

    def async[A](listen: Callback[A]) =
      Future.async(listen)
    def now[A](a: A) =
      Future.now(a)
  }

  implicit val TaskMonadAsync = new MonadAsync[Task] {
    protected def M = Monad[Task]

    def async[A](listen: (A => Unit) => Unit) =
      Task.async { callback => listen { a => callback(Task.Try(a)) } }
    def now[A](a: A) =
      Task.now(a)
  }

  implicit def EitherTMonadAsync[F[_]: MonadAsync, L] = new MonadAsync[EitherT[F, L, ?]] {
    private implicit val MF: Monad[F] = MonadAsync[F].monad

    def now[A](a: A): EitherT[F, L, A] =
      EitherT.right[F, L, A](a.now[F])
    def async[A](listen: Callback[A]): EitherT[F, L, A] =
      EitherT.right[F, L, A](listen.liftAsync[F])

    protected def M =
      Monad[EitherT[F, L, ?]]
  }

  implicit def WriterTMonadAsync[F[_]: MonadAsync, W: Monoid] = new MonadAsync[WriterT[F, W, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def now[A](a: A): WriterT[F, W, A] =
      put(a.now[F])
    def async[A](listen: Callback[A]): WriterT[F, W, A] =
      put(listen.liftAsync[F])

    protected def M =
      Monad[WriterT[F, W, ?]]
    private def put[A](fa: F[A]): WriterT[F, W, A] =
      WriterT.put(fa)(Monoid[W].zero)
  }

  implicit def ReaderTMonadAsync[F[_]: MonadAsync, E] = new MonadAsync[ReaderT[F, E, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def now[A](a: A): ReaderT[F, E, A] =
      kleisli { a.now[F] }
    def async[A](listen: Callback[A]): ReaderT[F, E, A] =
      kleisli { listen.liftAsync[F] }

    protected def M =
      Monad[ReaderT[F, E, ?]]
    private def kleisli[A](fa: => F[A]): ReaderT[F, E, A] =
      Kleisli.kleisli { _ => fa }
  }

  implicit def StateTMonadAsync[F[_]: MonadAsync, S] = new MonadAsync[StateT[F, S, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def now[A](a: A): StateT[F, S, A] =
      state { a.now[F] }
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