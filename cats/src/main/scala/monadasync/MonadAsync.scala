package monadasync

import java.util.concurrent.{ TimeUnit, ScheduledExecutorService, Executor }

import scala.concurrent.duration.Duration
import cats._
import cats.data._
import cats.syntax.all._

trait MonadAsync[F[_]] extends internal.MonadAsync[F] with MonadSuspend[F]

object MonadAsync extends MonadAsyncInstances {
  def apply[F[_]: MonadAsync]: MonadAsync[F] = macro imp.summon[MonadAsync[F]]

  //  def monadAsyncLaw[F[_]: Monad: Catchable](ma: MonadAsync[F]): MonadAsyncLaw[F] = new MonadAsyncLaw[F](ma) {}

  object syntax extends MonadAsyncFunctions {
    implicit def ToMonadAsyncOps[F[_]: MonadAsync: Monad: Catchable, A](v: F[A]): MonadAsyncOps[F, A] =
      new MonadAsyncOps[F, A](v)

    /**
     * Callback for operations that may return errors.
     */
    implicit class EitherAsync[L, A](val cb: Callback[L Xor A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], ME: MonadError[F, L]): F[A] =
        MA.async(cb) >>= {
          case Xor.Right(a) => ME.pure(a)
          case Xor.Left(l) => ME.raiseError(l)
        }
    }

    /**
     * Callback for operations depend on a configuration.
     */
    implicit class ReaderAsync[R, A](val f: R => Callback[A]) extends AnyVal {
      def liftAsync[F[_]](implicit MA: MonadAsync[F], MR: MonadReader[F, R]): F[A] =
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

trait MonadAsyncInstances {
  implicit def XorTMonadAsync[F[_]: MonadAsync: Monad, L]: MonadAsync[XorT[F, L, ?]] = new MonadAsync[XorT[F, L, ?]] {
    override def async[A](listen: ((A) => Unit) => Unit): XorT[F, L, A] =
      XorT(MonadAsync[F].async(listen) map Xor.right)
    override def now[A](a: A): XorT[F, L, A] =
      XorT(MonadAsync[F].now(a.right))
    override def delay[A](a: => A): XorT[F, L, A] =
      XorT(MonadAsync[F].delay(a.right))
    override def suspend[A](fa: => XorT[F, L, A]): XorT[F, L, A] =
      XorT(MonadAsync[F].suspend(fa.value))
  }
}

trait MonadAsyncFunctions extends MonadSuspendFunctions {
  /**
   * All operations against F after fork will execute in a thread within the given pool.
   */
  def fork[F[_]: Monad: MonadAsync: Catchable, A](fa: => F[A])(implicit pool: Executor): F[A] =
    async[F, F[A]](fa).flatten

  /**
   * @return an F[A] whose value will be set after a given delay, and future operation will execute in a thread within the given pool.
   */
  def schedule[F[_]: Monad: MonadAsync: Catchable, A](delay: Duration)(a: => A)(implicit pool: ScheduledExecutorService): F[A] =
    callback({ cb: (Throwable Xor A => Unit) =>
      try {
        pool.schedule(new Runnable { def run() = cb(Xor.catchNonFatal(a)) }, delay.toNanos, TimeUnit.NANOSECONDS)
        ()
      } catch {
        case t: Throwable => cb(Xor.left(t))
      }
    }).unattempt

  def callback[F[_], A](cb: Callback[A])(implicit MA: MonadAsync[F]): F[A] =
    MA.async(cb)

  def async[F[_]: Monad: MonadAsync: Catchable, A](a: => A)(implicit pool: Executor): F[A] =
    callback({ cb: (Throwable Xor A => Unit) =>
      try {
        pool.execute(new Runnable { def run() = cb(Xor.catchNonFatal(a)) })
        ()
      } catch {
        case t: Throwable => cb(Xor.left(t))
      }
    }).unattempt
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

/**
 * Some laws any MonadAsync implementation should obey.
 */
abstract class MonadAsyncLaws[F[_]: Monad: MonadAsync: Catchable] {
  import cats.laws._
  import MonadAsync.syntax._
  implicit val pool = SameThreadExecutor
  def asyncIsDelay[A](a: () => A): IsEq[F[A]] =
    MonadAsync[F].delay(a()) <-> async[F, A](a())
}

object MonadAsyncLaws {
  def monadAsyncLaw[F[_]: Monad: MonadAsync: Catchable]: MonadAsyncLaws[F] = new MonadAsyncLaws[F] {}
}

