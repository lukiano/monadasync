package io.atlassian.monadasync

import cats.data.XorT
import cats.state.StateT
import cats.syntax.functor._
import Future._
import MonadAsync._

trait Async {
  type Task[A] = XorT[Future, Throwable, A]
  type StateTask[A] = StateT[Task, Int, A]

  def run[A](f: StateTask[A]): A =
    f.runA(0).value.map(_.toOption).run.get

  val MonadAsyncF = MonadAsync[StateTask]
}
