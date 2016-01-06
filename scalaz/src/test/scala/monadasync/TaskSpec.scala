package monadasync
import scalaz._
import scalaz.concurrent.Task
import Nondeterminisms._
import MonadAsync.TaskMonadAsync

import scalaz.scalacheck.ScalazProperties

object TaskSpec extends MonadAsyncSpec[Task] {

  override def run[A](f: Task[A]): A =
    f.run

  val laws = MonadAsyncProperties.monadAsync.laws[Task]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[Task])
}
