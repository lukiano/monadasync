package io.atlassian.monadasync

import scalaz.concurrent.Future
import scalaz.syntax.either._

// borrowed from scalaz concurrent tests
object TimerSpec extends org.specs2.mutable.SpecificationWithJUnit {

  import MonadAsync.FutureMonadAsync

  def withTimer[T](expression: Timer => T): T = {
    val timer = Timer(10)
    try {
      expression(timer)
    } finally {
      timer.stop()
    }
  }

  "Timer" should {
    "stop normally".in {
      withTimer { timer => ok }
    }
    "handle stop being called repeatedly" in {
      withTimer { timer =>
        timer.stop()
        ok
      }
    }
    "valueWait produces a value after the specified timeout" in {
      withTimer { timer =>
        val start = System.currentTimeMillis
        WithTimeout(5000) {
          val future = timer.valueWait[Future, String]("Test", 100)
          (future.run === "Test") and ((System.currentTimeMillis - start) >= 100)
        }
      }
    }
    "withTimeout(Future...) produces a Timeout if the timeout is exceeded" in {
      withTimer { timer =>
        val future = timer.withTimeout(Future { Thread.sleep(500); "Test" }, 100)
        WithTimeout(5000) {
          future.run must_== Timeout().left
        }
      }
    }
    "produces the result of the Future if the timeout is not exceeded" in {
      withTimer { timer =>
        val future = timer.withTimeout(Future { Thread.sleep(100); "Test" }, 500)
        WithTimeout(5000) {
          future.run must_== "Test".right
        }
      }
    }
  }
}