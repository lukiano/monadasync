package io.atlassian.monadasync

import java.util.concurrent.atomic.AtomicBoolean

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
  implicit def monad: Monad[F]

  val monadSuspendSyntax = new MonadSuspend.MonadSuspendSyntax[F] {}

  /**
   * Some laws any MonadSuspend implementation should obey.
   */
  trait MonadSuspendLaw {
    def nowIsPoint[A](a: A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(now(a), a.point[F])
    def delayDoesntRun[A](a: A)(implicit FEA: Equal[F[A]]): Boolean = {
      val flag = new AtomicBoolean(false)
      def run: A = {
        flag.set(true)
        a
      }
      val fa = delay(run)
      if (flag.get()) {
        false
      } else {
        FEA.equal(fa, now(a)) && flag.get()
      }
    }
    def suspendIsDelayJoin[A](fa: F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(fa).join, suspend(fa))
  }
  def monadSuspendLaw: MonadSuspendLaw = new MonadSuspendLaw {}
}

object MonadSuspend {
  def apply[F[_]: MonadSuspend]: MonadSuspend[F] = macro imp.summon[MonadSuspend[F]]

  trait MonadSuspendSyntax[F[_]] {
    implicit def toMonadSuspendOps[A](v: F[A])(implicit F0: MonadSuspend[F]): MonadSuspendOps[F, A] =
      new MonadSuspendOps[F, A](v)(F0)
  }

  object syntax extends MonadSuspendFunctions {
    implicit def toMonadSuspendOps[F[_]: MonadSuspend, A](v: F[A]): MonadSuspendOps[F, A] =
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