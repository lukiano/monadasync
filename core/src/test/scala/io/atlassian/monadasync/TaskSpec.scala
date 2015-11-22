package io.atlassian.monadasync

import scalaz._
import scalaz.concurrent.Task
import Nondeterminisms._

object TaskSpec extends MonadAsyncSpec {

  override type F[A] = Task[A]

  override def run[A](f: F[A]): A =
    f.run

  override val MonadAsyncF = MonadAsync[F]

  override val NondeterminismF = Nondeterminism[F]

  val laws = MonadAsyncProperties.monadAsync.laws[F](
    MonadAsyncF,
    arbitraryTC,
    arbitraryInt,
    arbitraryF0,
    arbitraryF1,
    equalTc
  )

  checkAll("MonadAsync laws", laws)
}
