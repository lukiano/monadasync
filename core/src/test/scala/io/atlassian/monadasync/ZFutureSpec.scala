package io.atlassian.monadasync

import scalaz._
import scalaz.concurrent.Future

object ZFutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

  override def run[A](f: F[A]): A =
    f.run

  override val MonadAsyncF = MonadAsync[F]

  override val NondeterminismF = Nondeterminism[F]

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)
}
