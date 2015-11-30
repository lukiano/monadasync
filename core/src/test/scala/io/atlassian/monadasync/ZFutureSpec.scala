package io.atlassian.monadasync

import scalaz._
import scalaz.concurrent.Future
import scalaz.scalacheck.ScalazProperties

object ZFutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

  override def run[A](f: F[A]): A =
    f.run

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = new Catchable[F] {
    override def attempt[A](f: Future[A]): Future[Throwable \/ A] =
      f map \/-.apply
    override def fail[A](err: Throwable): Future[A] =
      ???
  }

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])
}
