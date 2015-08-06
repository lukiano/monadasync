package io.atlassian.monadasync

import scalaz._
import scalaz.std.anyVal._
import scalaz.concurrent.Future
import Future._
import MonadAsync._
import Nondeterminisms._

trait Async {
  type EitherFixed[A] = EitherT[Future, Throwable, A]
  type WriterFixed[A] = WriterT[EitherFixed, Int, A]
  type TC[A] = ReaderT[WriterFixed, Unit, A]

  def run[A](f: TC[A]): A =
    f.run(()).value.run.run.toOption.get

  val MonadAsyncF = MonadAsync[TC]

  val NondeterminismF = Nondeterminism[TC]
}
