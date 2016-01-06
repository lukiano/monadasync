package monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService, TimeUnit }

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent.{ Future, Task }
import scalaz.syntax.monad._

trait MonadAsync[F[_]] extends internal.MonadAsync[F] with MonadSuspend[F]

object MonadAsync extends MonadAsyncInstances {
  def apply[F[_]: MonadAsync]: MonadAsync[F] = macro imp.summon[MonadAsync[F]]

  /**
   * Some laws any MonadAsync implementation should obey.
   * TODO include nondeterminism
   */
  abstract class MonadAsyncLaw[F[_]: Monad: Catchable](ma: MonadAsync[F]) extends MonadSuspend.MonadSuspendLaw[F](ma) {
    import MonadAsync.syntax._
    implicit val impMA = ma
    implicit val pool = SameThreadExecutor
    def asyncIsDelay[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(ma.delay(a()), async[F, A](a()))
  }
  def monadAsyncLaw[F[_]: Monad: Catchable](ma: MonadAsync[F]): MonadAsyncLaw[F] = new MonadAsyncLaw[F](ma) {}

  object syntax extends MonadAsyncFunctions {
    implicit def ToMonadAsyncOps[F[_]: MonadAsync: Monad: Catchable, A](v: F[A]): MonadAsyncOps[F, A] =
      new MonadAsyncOps[F, A](v)

    /**
     * Callback for operations that may return errors.
     */
    implicit class EitherAsync[L, A](val cb: Callback[L \/ A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], ME: MonadError[λ[(α, β) => F[β]], L]): F[A] =
        MA.async(cb) >>= {
          case \/-(a) => a.point[F]
          case -\/(l) => ME.raiseError(l)
        }
    }

    /**
     * Callback for operations that may contain logs.
     */
    implicit class WriterAsync[W, A](val cb: Callback[(W, A)]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MT: MonadTell[λ[(α, β) => F[β]], W]): F[A] =
        MA.async(cb) >>= {
          case (w, a) => MT.writer(w, a)
        }
    }

    /**
     * Callback for operations depend on a configuration.
     */
    implicit class ReaderAsync[R, A](val f: R => Callback[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MR: MonadReader[λ[(α, β) => F[β]], R]): F[A] =
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
      def liftAsync[F[_]](implicit MA: MonadAsync[F], M: Monad[F], C: Catchable[F]): F[A] = {
        MA.async(t.callback).unattempt
      }
    }
    implicit def TaskTransformation[F[_]](implicit MA: MonadAsync[F], M: Monad[F], C: Catchable[F]): Task ~> F =
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
  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork[F[_]: Monad: MonadAsync: Catchable, A](fa: => F[A])(implicit pool: Executor): F[A] =
    async[F, F[A]](fa).join

  /**
   * @return an F[A] whose value will be set after a given delay, and future operation will execute in a thread within the given pool.
   */
  def schedule[F[_]: Monad: MonadAsync: Catchable, A](delay: Duration)(a: => A)(implicit pool: ScheduledExecutorService): F[A] =
    callback({ cb: (Throwable \/ A => Unit) =>
      try {
        pool.schedule(new Runnable { def run() = cb(Task.Try(a)) }, delay.toNanos, TimeUnit.NANOSECONDS)
        ()
      } catch {
        case t: Throwable => cb(-\/(t))
      }
    }).unattempt

  def callback[F[_], A](cb: Callback[A])(implicit MA: MonadAsync[F]): F[A] =
    MA.async(cb)

  def async[F[_]: Monad: MonadAsync: Catchable, A](a: => A)(implicit pool: Executor): F[A] =
    callback({ cb: (Throwable \/ A => Unit) =>
      try {
        pool.execute(new Runnable { def run() = cb(Task.Try(a)) })
        ()
      } catch {
        case t: Throwable => cb(-\/(t))
      }
    }).unattempt
}

trait MonadAsyncInstances {
  import MonadAsync.syntax._

  implicit object FutureMonadAsync extends MonadAsync[Future] {
    def async[A](listen: Callback[A]): Future[A] =
      Future.async(listen)
    def delay[A](a: => A): Future[A] =
      Future.delay(a)
    def now[A](a: A): Future[A] =
      Future.now(a)
    override def suspend[A](fa: => Future[A]): Future[A] =
      Future.suspend(fa)
  }

  implicit object TaskMonadAsync extends MonadAsync[Task] {
    def async[A](listen: (A => Unit) => Unit): Task[A] =
      Task.async { callback => listen { a => callback(Task.Try(a)) } }
    def delay[A](a: => A): Task[A] =
      Task.taskInstance.point(a)
    def now[A](a: A): Task[A] =
      Task.now(a)
    override def suspend[A](ta: => Task[A]): Task[A] =
      Task.suspend(ta)
  }

  class MonadTransMonadAsync[F[_]: MonadAsync: Monad, G[_[_], _]: MonadTrans] extends MonadAsync[λ[α => G[F, α]]] {
    final def delay[A](a: => A): G[F, A] =
      MonadAsync[F].delay(a).liftM[G]
    final def now[A](a: A): G[F, A] =
      a.now[F].liftM[G]
    final def suspend[A](a: => G[F, A]): G[F, A] =
      MonadTrans[G].apply[F].join(MonadAsync[F].delay(a).liftM[G])
    final def async[A](listen: Callback[A]): G[F, A] =
      listen.liftAsync[F].liftM[G]
  }

  def tripleMonadTransMonadAsync[F[_], Q, H[_[_], _, _]](implicit MA: MonadAsync[F], M: Monad[F], MT: MonadTrans[λ[(Φ[_], α) => H[Φ, Q, α]]]) =
    new MonadTransMonadAsync[F, λ[(Φ[_], α) => H[Φ, Q, α]]]

  implicit def EitherTMonadAsync[F[_]: MonadAsync: Monad, L]: MonadAsync[EitherT[F, L, ?]] =
    tripleMonadTransMonadAsync[F, L, EitherT]
  implicit def WriterTMonadAsync[F[_]: MonadAsync: Monad, W: Monoid]: MonadAsync[WriterT[F, W, ?]] =
    tripleMonadTransMonadAsync[F, W, WriterT]
  implicit def ReaderTMonadAsync[F[_]: MonadAsync: Monad, E]: MonadAsync[ReaderT[F, E, ?]] =
    tripleMonadTransMonadAsync[F, E, ReaderT]
  implicit def StateTMonadAsync[F[_]: MonadAsync: Monad, S]: MonadAsync[StateT[F, S, ?]] =
    tripleMonadTransMonadAsync[F, S, StateT]
}

final class MonadAsyncOps[F[_]: MonadAsync: Monad: Catchable, A](self: F[A]) extends MonadSuspendOps[F, A](self) {
  import MonadAsync.syntax.{ fork => forkMe }
  /**
   * Asynchronous bind
   */
  def flatMapA[B](f: A => F[B])(implicit pool: Executor): F[B] =
    fork flatMap f

  /**
   * Asynchronous map
   */
  def mapA[B](f: A => B)(implicit pool: Executor): F[B] =
    fork map f

  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork(implicit pool: Executor): F[A] =
    forkMe(self)

  /**
   * Continue running after d.
   */
  def after(d: Duration): F[A] =
    after(d.toMillis)

  /**
   * Continue running after t milliseconds.
   */
  def after(t: Long): F[A] = {
    Timer.default.valueWait[F, Unit]((), t) >> self
  }
}