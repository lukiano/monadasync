package io.atlassian.monadasync

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
   * @return an F whose value will be eventually computed.
   */
  def delay[A](a: => A): F[A]

  /**
   * @return an F[A] wrapped in a suspension that will be eventually computed.
   */
  def suspend[A](fa: => F[A]): F[A] =
    delay(fa).join // do not use *> from Apply[F] as we want an explicit sequence.

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
    def delayIsMap[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(a()), now(()) as a())
    def suspendIsDelayJoin[A](fa: F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(delay(fa).join, suspend(fa))
  }
  def monadSuspendLaw: MonadSuspendLaw = new MonadSuspendLaw {}
}

object MonadSuspend extends MonadSuspendInstances {
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

trait MonadSuspendInstances {

  import Free.Trampoline

  implicit object TrampolineMonadSuspend extends MonadSuspend[Trampoline] {
    def now[A](a: A): Trampoline[A] =
      Trampoline.done(a)
    def delay[A](a: => A): Trampoline[A] =
      Trampoline.delay(a)
    override def suspend[A](fa: => Trampoline[A]): Trampoline[A] =
      Trampoline.suspend(fa)

    implicit def monad: Monad[Trampoline] = Trampoline.trampolineInstance
  }

}

trait MonadSuspendFunctions {
  def tryCatch[F[_]: Monad: Catchable, A](a: => A): F[A] =
    Task.Try(a).point[F].unattempt

  def suspend[F[_], A](fa: => F[A])(implicit MA: MonadSuspend[F]): F[A] =
    MA.suspend(fa)

  def unit[F[_]: MonadSuspend]: F[Unit] =
    ().now[F]

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