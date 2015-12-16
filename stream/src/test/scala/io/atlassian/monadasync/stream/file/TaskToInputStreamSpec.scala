package io.atlassian.monadasync
package stream
package file

import org.junit.runner.RunWith
import scalaz._
import scalaz.concurrent.Task

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TaskToInputStreamSpec extends InputStreamSpec {

  type F[A] = Task[A]

  val Runner: F ~> Id.Id = new (F ~> Id.Id) {
    def apply[A](fr: F[A]): A =
      fr.run
  }

  val MS: MonadSuspend[F] = MonadAsync.TaskMonadAsync
  val C: Catchable[F] = Task.taskInstance
}

