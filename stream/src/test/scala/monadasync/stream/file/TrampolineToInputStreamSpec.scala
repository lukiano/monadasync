package monadasync
package stream
package file

import org.junit.runner.RunWith

import scalaz._
import scalaz.Free.Trampoline
import scalaz.Trampoline._
import MonadSuspend._
import Catchables._

object Type {
  type Task[A] = EitherT[Trampoline, Throwable, A]
}

import Type._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TrampolineToInputStreamSpec extends InputStreamSpec[Task](
  new (Task ~> Id.Id) {
    def apply[A](ta: Task[A]): A =
      ta.run.run match {
        case \/-(a) => a
        case -\/(t) => throw t
      }
  }
)

