package monadasync

import cats._
import free.Trampoline

trait MonadSuspend[F[_]] extends internal.MonadSuspend[F]

object MonadSuspend extends MonadSuspendInstances {
  def apply[F[_]: MonadSuspend]: MonadSuspend[F] = macro imp.summon[MonadSuspend[F]]

  object syntax extends MonadSuspendFunctions {
    implicit def toMonadSuspendOps[F[_]: MonadSuspend: Monad, A](v: F[A]): MonadSuspendOps[F, A] =
      new MonadSuspendOps[F, A](v)
  }

  /**
   * Some laws any MonadSuspend implementation should obey.
   */
  abstract class MonadSuspendLaw[F[_]: Monad](ms: MonadSuspend[F]) {
    //    def nowIsPoint[A](a: A): IsEq[F[A]] =
    //      ms.now(a) <-> a.point[F]
    //    def delayIsMap[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
    //      FEA.equal(ms.delay(a()), ms.now(()) as a())
    //    def suspendIsDelayJoin[A](fa: F[A])(implicit FEA: Equal[F[A]]): Boolean =
    //      FEA.equal(ms.delay(fa).join, ms.suspend(fa))
  }

  def monadSuspendLaw[F[_]: Monad](ms: MonadSuspend[F]): MonadSuspendLaw[F] = new MonadSuspendLaw[F](ms) {}
}

trait MonadSuspendFunctions {
  def tryCatch[F[_]: Monad: Catchable, A](a: => A)(implicit MS: MonadSuspend[F]): F[A] =
    MS.suspend {
      try a.now[F] catch { case e: Throwable => fail[F, A](e) }
    }

  def suspend[F[_], A](fa: => F[A])(implicit MS: MonadSuspend[F]): F[A] =
    MS.suspend(fa)

  def delay[F[_], A](a: => A)(implicit MS: MonadSuspend[F]): F[A] =
    MS.delay(a)

  def unit[F[_]: MonadSuspend]: F[Unit] =
    ().now

  /**
   * Any to F.
   */
  implicit class AnyNow[A](val a: A) {
    def now[F[_]](implicit MS: MonadSuspend[F]): F[A] =
      MS.now(a)
  }
}

trait MonadSuspendInstances {
  implicit object EvalMonadSuspend extends MonadSuspend[Eval] {
    override def now[A](a: A): Eval[A] =
      Eval.now(a)
    override def delay[A](a: => A): Eval[A] =
      Eval.always(a)
    override def suspend[A](fa: => Eval[A]): Eval[A] =
      Eval.defer(fa)
  }

  implicit object TrampolineMonadSuspend extends MonadSuspend[Trampoline] {
    override def now[A](a: A): Trampoline[A] =
      Trampoline.done(a)
    override def delay[A](a: => A): Trampoline[A] =
      Trampoline.delay(a)
    override def suspend[A](fa: => Trampoline[A]): Trampoline[A] =
      Trampoline.suspend(fa)
  }

  // If it makes sense for such F ...
  def fromMonad[F[_]](implicit M: Monad[F]): MonadSuspend[F] = new MonadSuspend[F] {
    def now[A](a: A): F[A] =
      M.pure(a)
    def delay[A](a: => A): F[A] =
      M.map(now(()))(_ => a)
    override def suspend[A](fa: => F[A]): F[A] =
      M.flatMap(now(()))(_ => fa)
  }

}

class MonadSuspendOps[F[_], A](self: F[A])(implicit MS: MonadSuspend[F], M: Monad[F]) {
}
