package io.atlassian.monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService, TimeUnit }

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent.{ Future, Task }
import scalaz.syntax.monad._

/**
 * Provides asynchronous operations for F
 */
trait MonadAsync[F[_]] extends MonadSuspend[F] {

  /**
   * @return an F whose value will be set from an asynchronous computation, via callback.
   */
  def async[A](listen: Callback[A]): F[A]

  /**
   * @return an F whose value will be computed when called, in a thread within the given pool.
   */
  def async[A](a: => A)(implicit pool: Executor): F[A] =
    async { cb =>
      pool.execute(cb(a))
    }

  /**
   * Asynchronous bind. f executes in a thread within the given pool.
   */
  def bindA[A, B](fa: F[A])(f: A => F[B])(implicit pool: Executor): F[B] =
    fork(fa) >>= f

  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork[A](fa: => F[A])(implicit pool: Executor): F[A] =
    async(fa).join

  /**
   * Asynchronous map. f executes in a thread within the given pool.
   */
  def mapA[A, B](fa: F[A])(f: A => B)(implicit pool: Executor): F[B] =
    fork(fa) map f

  /**
   * @return an F[A] whose value will be set after a given delay, and future operation will execute in a thread within the given pool.
   */
  def schedule[A](a: => A, delay: Duration)(implicit pool: ScheduledExecutorService): F[A] =
    async { cb =>
      pool.schedule(cb(a), delay.toMillis, TimeUnit.MILLISECONDS)
      ()
    }

  private implicit def AsRunnable(block: => Unit): Runnable =
    new Runnable { def run() = block }

  val monadAsyncSyntax = new MonadAsync.MonadAsyncSyntax[F] {}

  /**
   * Some laws any MonadAsync implementation should obey.
   * TODO include nondeterminism
   */
  trait MonadAsyncLaw extends MonadSuspendLaw {
    def asyncIsDelay[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(a()), async(a())(MonadAsync.SameThreadExecutor))
    def bindAIsBind[A, B](a: A, f: A => F[A], b: A => F[B])(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) >>= b, bindA(f(a))(b)(MonadAsync.SameThreadExecutor))
    def mapAIsMap[A, B](a: A, f: A => F[A], b: A => B)(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) ∘ b, mapA(f(a))(b)(MonadAsync.SameThreadExecutor))
  }
  def monadAsyncLaw: MonadAsyncLaw = new MonadAsyncLaw {}
}

object MonadAsync extends MonadAsyncInstances {
  def apply[F[_]: MonadAsync]: MonadAsync[F] = macro imp.summon[MonadAsync[F]]

  trait MonadAsyncSyntax[F[_]] {
    implicit def ToMonadAsyncOps[A](v: F[A])(implicit F0: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)(F0)
  }

  /**
   * Used to test laws.
   */
  object SameThreadExecutor extends Executor {
    def execute(command: Runnable): Unit = command.run()
  }

  object syntax extends MonadAsyncFunctions {
    implicit def ToMonadAsyncOps[F[_], A](v: F[A])(implicit MA: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)

    /**
     * Callback for operations that may return errors.
     */
    implicit class EitherAsync[L, A](val cb: Callback[L \/ A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], ME: MonadError[Lambda[(α, β) => F[β]], L]): F[A] =
        MA.async(cb) >>= {
          case \/-(a) => a.point[F]
          case -\/(l) => ME.raiseError(l)
        }
    }

    /**
     * Callback for operations that may contain logs.
     */
    implicit class WriterAsync[W, A](val cb: Callback[(W, A)]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MT: MonadTell[Lambda[(α, β) => F[β]], W]): F[A] =
        MA.async(cb) >>= {
          case (w, a) => MT.writer(w, a)
        }
    }

    /**
     * Callback for operations depend on a configuration.
     */
    implicit class ReaderAsync[R, A](val f: R => Callback[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MR: MonadReader[Lambda[(α, β) => F[β]], R]): F[A] =
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
     * scalaz.Future ~> F
     */
    implicit class ScalazFutureAsync[A](val f: Future[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F]): F[A] =
        f.callback.liftAsync[F]
    }
    implicit def ScalazFutureTransformation[F[_]](implicit MA: MonadAsync[F]): Future ~> F =
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

trait MonadAsyncFunctions extends MonadSuspendFunctions {
  def schedule[F[_], A](delay: Duration)(a: => A)(implicit MA: MonadAsync[F], pool: ScheduledExecutorService): F[A] =
    MA.schedule(a, delay)

  def asyncCatch[F[_], A](a: => A)(implicit MA: MonadAsync[F], C: Catchable[F], pool: Executor): F[A] = {
    implicit val M = MA.monad
    MA.async(Task.Try(a)).unattempt
  }
}

trait MonadAsyncInstances {
  import MonadAsync.syntax._

  implicit val FutureMonadAsync = new MonadAsync[Future] {
    def monad = Monad[Future]

    def async[A](listen: Callback[A]) =
      Future.async(listen)
    def delay[A](a: => A) =
      Future.delay(a)
    def now[A](a: A) =
      Future.now(a)
    override def suspend[A](fa: => Future[A]) =
      Future.suspend(fa)
  }

  implicit val TaskMonadAsync = new MonadAsync[Task] {
    override val monad = Monad[Task]

    def async[A](listen: (A => Unit) => Unit) =
      Task.async { callback => listen { a => callback(Task.Try(a)) } }
    def delay[A](a: => A) =
      Task.delay(a)
    def now[A](a: A) =
      Task.now(a)
    override def suspend[A](ta: => Task[A]) =
      Task.suspend(ta)
  }

  implicit def EitherTMonadAsync[F[_]: MonadAsync, L] = new MonadAsync[EitherT[F, L, ?]] {
    private implicit val MF: Monad[F] = MonadAsync[F].monad

    def delay[A](a: => A): EitherT[F, L, A] =
      EitherT.right[F, L, A](MonadAsync[F].delay(a))
    def now[A](a: A): EitherT[F, L, A] =
      EitherT.right[F, L, A](a.now[F])
    def async[A](listen: Callback[A]): EitherT[F, L, A] =
      EitherT.right[F, L, A](listen.liftAsync[F])

    override def monad =
      Monad[EitherT[F, L, ?]]
  }

  implicit def WriterTMonadAsync[F[_]: MonadAsync, W: Monoid] = new MonadAsync[WriterT[F, W, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def delay[A](a: => A): WriterT[F, W, A] =
      put(MonadAsync[F].delay(a))
    def now[A](a: A): WriterT[F, W, A] =
      put(a.now[F])
    def async[A](listen: Callback[A]): WriterT[F, W, A] =
      put(listen.liftAsync[F])

    override val monad =
      Monad[WriterT[F, W, ?]]
    private def put[A](fa: F[A]): WriterT[F, W, A] =
      WriterT.put(fa)(Monoid[W].zero)
  }

  implicit def ReaderTMonadAsync[F[_]: MonadAsync, E] = new MonadAsync[ReaderT[F, E, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def delay[A](a: => A): ReaderT[F, E, A] =
      reader { MonadAsync[F].delay(a) }
    def now[A](a: A): ReaderT[F, E, A] =
      reader { a.now[F] }
    def async[A](listen: Callback[A]): ReaderT[F, E, A] =
      reader { listen.liftAsync[F] }

    override val monad =
      Monad[ReaderT[F, E, ?]]
    private def reader[A](fa: => F[A]): ReaderT[F, E, A] =
      Kleisli.kleisli { _ => fa }
  }

  implicit def StateTMonadAsync[F[_]: MonadAsync, S] = new MonadAsync[StateT[F, S, ?]] {
    private implicit val MF = MonadAsync[F].monad

    def delay[A](a: => A): StateT[F, S, A] =
      state { MonadAsync[F].delay(a) }
    def now[A](a: A): StateT[F, S, A] =
      state { a.now[F] }
    def async[A](listen: Callback[A]): StateT[F, S, A] =
      state { listen.liftAsync[F] }

    override val monad =
      Monad[StateT[F, S, ?]]
    private def state[A](fa: => F[A]): StateT[F, S, A] =
      StateT { s => fa map { s -> _ } }
  }
}

final class MonadAsyncOps[F[_], A](self: F[A])(implicit MA: MonadAsync[F]) extends MonadSuspendOps[F, A](self)(MA) {
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