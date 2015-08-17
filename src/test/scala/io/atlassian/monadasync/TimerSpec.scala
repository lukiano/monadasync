package io.atlassian.monadasync

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
  }
}