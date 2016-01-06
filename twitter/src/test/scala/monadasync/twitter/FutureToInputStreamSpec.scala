package monadasync
package twitter

import com.twitter.conversions.time._
import com.twitter.util.{ Await, Future }
import org.junit.runner.RunWith

import stream.file.InputStreamSpec
import scalaz._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class FutureToInputStreamSpec extends InputStreamSpec[Future](
  new (Future ~> Id.Id) {
    def apply[A](fr: Future[A]): A =
      Await.result(fr, 5 seconds)
  }
)

