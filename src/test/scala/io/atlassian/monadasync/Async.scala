package io.atlassian.monadasync

import cats.data.XorT
import cats.state.StateT
import cats.syntax.functor._
import Future._
import MonadAsync._

trait Async {
  type EitherFixed[A] = XorT[Future, Throwable, A]
  type TC[A] = StateT[EitherFixed, Int, A]

  def run[A](f: TC[A]): A =
    f.runA(0).value.map(_.toOption).run.get

  val MonadAsyncF = MonadAsync[TC]
}
