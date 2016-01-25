package monadasync
package twitter

import com.twitter.util.{ Await, Future }
import com.twitter.conversions.time._

object FutureCircuitBreakerSpec extends CircuitBreakerSpec[Future] {
  def run[A](f: Future[A]): A = Await.result(f, awaitTimeoutMs.millis)
}

object FutureTimerSpec extends TimerSpec[Future] {
  def run[A](f: Future[A]): A = Await.result(f, 1000.millis)
}

