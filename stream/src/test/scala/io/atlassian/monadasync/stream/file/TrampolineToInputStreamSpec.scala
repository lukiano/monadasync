package io.atlassian.monadasync
package stream
package file

import org.junit.runner.RunWith

import scalaz._
import scalaz.Free.Trampoline

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TrampolineToInputStreamSpec extends InputStreamSpec {
  import MonadSuspend._
  import Catchables._

  type F[A] = EitherT[Trampoline, Throwable, A]

  val Runner: F ~> Id.Id = new (F ~> Id.Id) {
    def apply[A](fr: F[A]): A =
      fr.run.run match {
        case \/-(a) => a
        case -\/(t) => throw t
      }
  }

  val MS: MonadSuspend[F] = MonadSuspend[F]
  val C: Catchable[F] = Catchable[F]
}

