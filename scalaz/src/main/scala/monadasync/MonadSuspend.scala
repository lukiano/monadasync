package monadasync

import scalaz._
import scalaz.concurrent.Task
import scalaz.syntax.monad._

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
    def nowIsPoint[A](a: A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(ms.now(a), a.point[F])
    def delayIsMap[A](a: () => A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(ms.delay(a()), ms.now(()) as a())
    def suspendIsDelayJoin[A](fa: F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(ms.delay(fa).join, ms.suspend(fa))
  }

  def monadSuspendLaw[F[_]: Monad](ms: MonadSuspend[F]): MonadSuspendLaw[F] = new MonadSuspendLaw[F](ms) {}
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
  }

  class MonadTransMonadSuspend[F[_]: MonadSuspend: Monad, G[_[_], _]: MonadTrans] extends MonadSuspend[λ[α => G[F, α]]] {
    final def delay[A](a: => A): G[F, A] =
      MonadSuspend[F].delay(a).liftM[G]
    final def now[A](a: A): G[F, A] =
      MonadSuspend[F].now(a).liftM[G]
    final def suspend[A](a: => G[F, A]): G[F, A] =
      MonadTrans[G].apply[F].join(MonadSuspend[F].delay(a).liftM[G])
  }

  def tripleMonadTransMonadSuspend[F[_]: MonadSuspend: Monad, Q, H[_[_], _, _]](implicit MT: MonadTrans[λ[(Φ[_], α) => H[Φ, Q, α]]]) =
    new MonadTransMonadSuspend[F, λ[(Φ[_], α) => H[Φ, Q, α]]]

  implicit def EitherTMonadSuspend[F[_]: MonadSuspend: Monad, L]: MonadSuspend[EitherT[F, L, ?]] =
    tripleMonadTransMonadSuspend[F, L, EitherT]
  implicit def WriterTMonadSuspend[F[_]: MonadSuspend: Monad, W: Monoid]: MonadSuspend[WriterT[F, W, ?]] =
    tripleMonadTransMonadSuspend[F, W, WriterT]
  implicit def ReaderTMonadSuspend[F[_]: MonadSuspend: Monad, E]: MonadSuspend[ReaderT[F, E, ?]] =
    tripleMonadTransMonadSuspend[F, E, ReaderT]
  implicit def StateTMonadSuspend[F[_]: MonadSuspend: Monad, S]: MonadSuspend[StateT[F, S, ?]] =
    tripleMonadTransMonadSuspend[F, S, StateT]
}

trait MonadSuspendFunctions {
  def tryCatch[F[_]: Monad: Catchable, A](a: => A)(implicit MS: MonadSuspend[F]): F[A] =
    MS.delay(Task.Try(a)).unattempt

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

class MonadSuspendOps[F[_], A](self: F[A])(implicit MS: MonadSuspend[F], M: Monad[F]) {
}