package io.atlassian.monadasync

import scalaz._
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import Nondeterminisms._

object SFutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

  override def run[A](f: F[A]): A =
    Await.result(f, 2 seconds)

  override val MonadAsyncF = MonadAsync[F]

  override val NondeterminismF = Nondeterminism[F]

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)
}
