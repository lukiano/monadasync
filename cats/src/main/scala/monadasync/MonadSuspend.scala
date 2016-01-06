package monadasync

import cats.Eval

trait MonadSuspend[F[_]] extends internal.MonadSuspend[F]

object MonadSuspend extends MonadSuspendInstances {
  def apply[F[_]: MonadSuspend]: MonadSuspend[F] = macro imp.summon[MonadSuspend[F]]
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
}
