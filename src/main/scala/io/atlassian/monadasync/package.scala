package io.atlassian

import java.util.concurrent.Executor

package object monadasync {

  type Callback[A] = (A => Unit) => Unit

  implicit class FutureToCallback[A](val f: Future[A]) extends AnyVal {
    def callback: Callback[A] = f.runAsync
  }

  val DefaultExecutor: Executor = scala.concurrent.ExecutionContext.Implicits.global
}
