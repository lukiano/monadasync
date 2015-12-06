package io.atlassian.monadasync

import java.util.concurrent.CompletableFuture

import JavaFuture._

import scalaz._
import scalaz.scalacheck.ScalazProperties

object JavaFutureSpec extends MonadAsyncSpec {

  override type F[A] = CompletableFuture[A]

  override def run[A](f: F[A]): A =
    Comonad[F].copoint(f)

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = Catchable[F]

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)

  checkAll("Comonad laws", ScalazProperties.comonad.laws[F])

  checkAll("Zip laws", ScalazProperties.zip.laws[F])

  //  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => F[A]], Throwable])
}
