package io.atlassian.monadasync
package twitter

import com.twitter.conversions.time._
import com.twitter.util.{ Await, Future }
import org.junit.runner.RunWith

import stream.file.InputStreamSpec
import scalaz._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class FutureToInputStreamSpec extends InputStreamSpec {

  type F[A] = Future[A]

  val Runner: F ~> Id.Id = new (F ~> Id.Id) {
    def apply[A](fr: F[A]): A =
      Await.result(fr, 5 seconds)
  }

  val MS: MonadSuspend[F] = FutureMonad
  val C: Catchable[F] = FutureMonad
}

