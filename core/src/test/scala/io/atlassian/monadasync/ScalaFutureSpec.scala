package io.atlassian.monadasync

import scalaz._
import scala.concurrent.Future
import ScalaFuture._

import scalaz.scalacheck.ScalazProperties

object ScalaFutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

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

  //  checkAll("MonadPlus laws", ScalazProperties.monadPlus.strongLaws[F])
}