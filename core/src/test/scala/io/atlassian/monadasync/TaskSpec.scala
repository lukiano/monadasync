package io.atlassian.monadasync

import scalaz._
import scalaz.concurrent.Task
import Nondeterminisms._

import scalaz.scalacheck.ScalazProperties

object TaskSpec extends MonadAsyncSpec {

  override type F[A] = Task[A]

  override def run[A](f: F[A]): A =
    f.run

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = Catchable[F]

  val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])
}
