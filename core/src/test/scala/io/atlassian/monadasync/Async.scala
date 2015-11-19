package io.atlassian.monadasync

import scalaz._
import scalaz.std.anyVal._
import scalaz.concurrent.Future
import Future._
import MonadAsync._
import Nondeterminisms._

trait Async {
  type Task[A] = EitherT[Future, Throwable, A]
  type WrittenTask[A] = WriterT[Task, Int, A]
  type Example[A] = ReaderT[WrittenTask, Unit, A]

  def run[A](f: Example[A]): A =
    f.run(()).value.run.run.toOption.get

  val MonadAsyncF = MonadAsync[Example]

  val NondeterminismF = Nondeterminism[Example]
}
