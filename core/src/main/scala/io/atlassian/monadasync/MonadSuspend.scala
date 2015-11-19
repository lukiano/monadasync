package io.atlassian.monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService, TimeUnit }

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent.Task
import scalaz.syntax.monad._

/**
 * Provides asynchronous operations for F
 */
trait MonadSuspend[F[_]] {

  /**
    * @return an F whose value is immediately set.
    */
  def now[A](a: A): F[A]

  /**
    * @return an F whose value will be computed when called, on the caller thread.
    */
  def delay[A](a: => A): F[A]

  /**
    * @return an F[A] wrapped in a suspension to be computed when called, on the caller thread.
    */
  def suspend[A](fa: => F[A]): F[A] =
    delay(fa).join

  /**
    * @return the underlying monad.
    */
  def monad: Monad[F] = M

  protected implicit def M: Monad[F]

  private implicit def AsRunnable(block: => Unit): Runnable =
    new Runnable { def run() = block }

  val monadAsyncSyntax = new MonadAsync.MonadAsyncSyntax[F] {}

  /**
   * Some laws any MonadAsync implementation should obey.
   * TODO include nondeterminism
   */
  trait MonadSuspendLaw {
    def asyncIsDelay[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(a()), async(a())(MonadAsync.SameThreadExecutor))
    def bindAIsBind[A, B](a: A, f: A => F[A], b: A => F[B])(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) >>= b, bindA(f(a))(b)(MonadAsync.SameThreadExecutor))
    def mapAIsMap[A, B](a: A, f: A => F[A], b: A => B)(implicit FEA: Equal[F[B]]): Boolean =
      FEA.equal(f(a) map b, mapA(f(a))(b)(MonadAsync.SameThreadExecutor))
  }
  def monadAsyncLaw = new MonadAsyncLaw {}
}

object MonadSuspend extends MonadSuspendInstances {
  def apply[F[_]: MonadAsync] = implicitly[MonadAsync[F]]

  trait MonadAsyncSyntax[F[_]] {
    implicit def ToMonadAsyncOps[A](v: F[A])(implicit F0: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)(F0)
  }

  object syntax extends MonadAsyncFunctions {
    implicit def ToMonadAsyncOps[F[_], A](v: F[A])(implicit MA: MonadAsync[F]) =
      new MonadAsyncOps[F, A](v)

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

  def tryCatch[F[_]: Monad: Catchable, A](a: => A): F[A] =
    Task.Try(a).point[F].unattempt

  def asyncCatch[F[_], A](a: => A)(implicit MA: MonadAsync[F], C: Catchable[F], pool: Executor): F[A] = {
    implicit val M = MA.monad
    MA.async(Task.Try(a)).unattempt
  }

  def suspend[F[_], A](fa: => F[A])(implicit MA: MonadAsync[F]): F[A] =
    MA.suspend(fa)
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