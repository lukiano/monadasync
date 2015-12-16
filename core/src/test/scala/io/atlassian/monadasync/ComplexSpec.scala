package io.atlassian.monadasync

import scalaz._
import scalaz.scalacheck.ScalazProperties
import scalaz.std.anyVal._
import scalaz.concurrent.Future
import Future._
import MonadAsync._
import Nondeterminisms._

object ComplexSpec extends MonadAsyncSpec {
  type Task[A] = EitherT[Future, Throwable, A]
  type WrittenTask[A] = WriterT[Task, Int, A]
  override type F[A] = ReaderT[WrittenTask, Unit, A]

  override def run[A](f: F[A]): A =
    f.run(()).value.run.run.toOption.get

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = {
    import Catchables._

    Kleisli.kleisliCatchable[WrittenTask, Unit]
  }

  override val laws = MonadAsyncProperties.monadAsync.laws[F](
    MonadAsyncF,
    arbitraryTC,
    arbitraryInt,
    arbitraryF0,
    arbitraryF1,
    equalTc
  )

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])
}
