package monadasync

import java.util.concurrent.{TimeUnit, CountDownLatch}

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scalaz.{Catchable, Nondeterminism}
import ScalaFuture._
import MonadAsync.syntax._

import scala.reflect.ClassTag

object ScalaFutureCircuitBreakerSpec extends CircuitBreakerSpec[scala.concurrent.Future] {
  def run[A](f: scala.concurrent.Future[A]): A = scala.concurrent.Await.result(f, awaitTimeout)
}

abstract class CircuitBreakerSpec[F[_]: MonadAsync: Nondeterminism: Catchable] extends org.specs2.mutable.SpecificationWithJUnit {

  def run[A](f: F[A]): A

  private def fail[A](t: Throwable) = Catchable[F].fail[A](t)

  "An asynchronous circuit breaker that is open" should {
    "throw exceptions when called before reset timeout" in {
      val breaker = longResetTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.openLatch) and
        intercept[CircuitBreakerOpenException] { run(breaker().withCircuitBreaker(delay[F, String](sayHi))) }
    }

    "transition to half-open on reset timeout" in {
      val breaker = shortResetTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.halfOpenLatch)
    }
  }

  "An asynchronous circuit breaker that is half-open" should {
    "pass through next call and close on success" in {
      val breaker = shortResetTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.halfOpenLatch) and
        (run(breaker().withCircuitBreaker(delay[F, String](sayHi))) must_===("hi")) and
        checkLatch(breaker.closedLatch)
    }

    "re-open on exception in call" in {
      val breaker = shortResetTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.halfOpenLatch) and
        intercept[TestException]({ run(breaker().withCircuitBreaker(fail(new TestException))) }) and
        checkLatch(breaker.openLatch)
    }

    "re-open on async failure" in {
      val breaker = shortResetTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.halfOpenLatch) and {
        breaker().withCircuitBreaker(fail(new TestException))
        checkLatch(breaker.openLatch)
      }
    }
  }

  "An asynchronous circuit breaker that is closed" should {
    "allow calls through" in {
      val breaker = longCallTimeoutCb()
      run(breaker().withCircuitBreaker(delay[F, String](sayHi))) must_===("hi")
    }

    "increment failure count on exception" in {
      val breaker = longCallTimeoutCb()
      intercept[TestException]({ run(breaker().withCircuitBreaker(fail(new TestException))) }) and
        checkLatch(breaker.openLatch) and
        (breaker().currentFailureCount must_===(1))
    }

    "increment failure count on async failure" in {
      val breaker = longCallTimeoutCb()
      breaker().withCircuitBreaker(fail(new TestException))
      checkLatch(breaker.openLatch) and
        (breaker().currentFailureCount must_===(1))
    }

    "reset failure count after success" in {
      val breaker = multiFailureCb()
      breaker().withCircuitBreaker(delay[F, String](sayHi))
      for (n <- 1 to 4) breaker().withCircuitBreaker(fail(new TestException))
      awaitCond(breaker().currentFailureCount == 4) and {
        breaker().withCircuitBreaker(delay[F, String](sayHi))
        awaitCond(breaker().currentFailureCount == 0)
      }
    }

    "increment failure count on callTimeout" in {
      val breaker = shortCallTimeoutCb()

      val fut = breaker().withCircuitBreaker(delay[F, Any] {
        Thread.sleep(150.millis.toMillis)
        throwException
      })
      checkLatch(breaker.openLatch)
      (breaker().currentFailureCount must_===(1)) and
      // Since the timeout should have happend before the inner code finishes
      // we expect a timeout, not TestException
      intercept[TimeoutException] {
        run(fut)
      }

    }
  }

  private def shortCallTimeoutCb(): Breaker =
    new Breaker(new CircuitBreaker[F](1, 50.millis, 500.millis))

  private def shortResetTimeoutCb(): Breaker =
    new Breaker(new CircuitBreaker[F](1, 1000.millis, 50.millis))

  private def longCallTimeoutCb(): Breaker =
    new Breaker(new CircuitBreaker[F](1, 5.seconds, 500.millis))

  private val longResetTimeout = 5.seconds
  private def longResetTimeoutCb(): Breaker =
    new Breaker(new CircuitBreaker[F](1, 100.millis, longResetTimeout))

  private def multiFailureCb(): Breaker =
    new Breaker(new CircuitBreaker[F](5, 200.millis, 500.millis))

  private def intercept[E <: Throwable](block: => Any)(implicit m: ClassTag[E]) = block must throwA[E]
  
  private def throwException = throw new TestException

  private def sayHi = "hi"

  protected final val awaitTimeout = 2.seconds

  private def checkLatch(latch: TestLatch) = {
    latch.check()
    ok
  }

  private def now: Duration = Duration(System.nanoTime(), TimeUnit.NANOSECONDS)

  private def awaitCond(p: => Boolean, max: FiniteDuration = 1.minute, interval: FiniteDuration = 100.millis, message: String = "") = {
    val stop = now + max

    @scala.annotation.tailrec
    def poll(t: Duration) {
      if (!p) {
        assert(now < stop, s"timeout $max expired: $message")
        Thread.sleep(t.toMillis)
        poll((stop - now) min interval)
      }
    }
    poll(max min interval)
    ok
  }

  private class TestException extends RuntimeException

  private class Breaker(val instance: CircuitBreaker[F]) {
    val halfOpenLatch = new TestLatch(1)
    val openLatch = new TestLatch(1)
    val closedLatch = new TestLatch(1)
    def apply(): CircuitBreaker[F] = instance
    instance.onClose(closedLatch.countDown()).onHalfOpen(halfOpenLatch.countDown()).onOpen(openLatch.countDown())
  }

  class TestLatch(count: Int = 1) {
    private var latch = new CountDownLatch(count)

    def countDown(): Unit = latch.countDown()
    def isOpen: Boolean = latch.getCount == 0
    def open(): Unit = while (!isOpen) countDown()
    def reset(): Unit = latch = new CountDownLatch(count)

    def check(): Unit = {
      val opened = latch.await(awaitTimeout.toNanos, TimeUnit.NANOSECONDS)
      if (!opened) throw new TimeoutException(
        "Timeout of %s" format awaitTimeout.toString)
    }
  }
}
