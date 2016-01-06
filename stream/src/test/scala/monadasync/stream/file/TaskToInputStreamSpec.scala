package monadasync
package stream
package file

import org.junit.runner.RunWith
import scalaz._
import scalaz.concurrent.Task

import MonadAsync.TaskMonadAsync
import Task.taskInstance

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TaskToInputStreamSpec extends InputStreamSpec[Task](
  new (Task ~> Id.Id) {
    def apply[A](fr: Task[A]): A =
      fr.run
  }
)

