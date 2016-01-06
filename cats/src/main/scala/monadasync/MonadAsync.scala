package monadasync

trait MonadAsync[F[_]] extends internal.MonadAsync[F] with MonadSuspend[F]

object MonadAsync extends MonadAsyncInstances {
  def apply[F[_] : MonadAsync]: MonadAsync[F] = macro imp.summon[MonadAsync[F]]
}

trait MonadAsyncInstances {

}
