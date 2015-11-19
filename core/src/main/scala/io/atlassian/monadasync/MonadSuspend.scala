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

  val monadSuspendSyntax = new MonadSuspend.MonadSuspendSyntax[F] {}

  /**
   * Some laws any MonadAsync implementation should obey.
   * TODO include nondeterminism
   */
  //  trait MonadSuspendLaw {
  //    def asyncIsDelay[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
  //      FEA.equal(delay(a()), async(a())(MonadAsync.SameThreadExecutor))
  //    def bindAIsBind[A, B](a: A, f: A => F[A], b: A => F[B])(implicit FEA: Equal[F[B]]): Boolean =
  //      FEA.equal(f(a) >>= b, bindA(f(a))(b)(MonadAsync.SameThreadExecutor))
  //    def mapAIsMap[A, B](a: A, f: A => F[A], b: A => B)(implicit FEA: Equal[F[B]]): Boolean =
  //      FEA.equal(f(a) map b, mapA(f(a))(b)(MonadAsync.SameThreadExecutor))
  //  }
  //  def monadAsyncLaw = new MonadAsyncLaw {}
}

object MonadSuspend {
  def apply[F[_]: MonadSuspend]: MonadSuspend[F] = macro imp.summon[MonadSuspend[F]]

  trait MonadSuspendSyntax[F[_]] {
    implicit def ToMonadSuspendOps[A](v: F[A])(implicit F0: MonadSuspend[F]) =
      new MonadSuspendOps[F, A](v)(F0)
  }

  object syntax extends MonadSuspendFunctions {
    implicit def ToMonadSuspendOps[F[_]: MonadSuspend, A](v: F[A]) =
      new MonadSuspendOps[F, A](v)
  }
}

trait MonadSuspendFunctions {
  def tryCatch[F[_]: Monad: Catchable, A](a: => A): F[A] =
    Task.Try(a).point[F].unattempt

  def suspend[F[_], A](fa: => F[A])(implicit MA: MonadAsync[F]): F[A] =
    MA.suspend(fa)

  /**
   * Any to F.
   */
  implicit class AnyNow[A](val a: A) {
    def now[F[_]](implicit MS: MonadSuspend[F]): F[A] =
      MS.now(a)
  }

}

class MonadSuspendOps[F[_], A](self: F[A])(implicit MS: MonadSuspend[F]) {
}