package monadasync

import scalaz.{ Catchable, Monad, Nondeterminism }
import scalaz.concurrent.Task
import scalaz.syntax.either._

import scala.concurrent.ExecutionContext.Implicits.global

// borrowed from scalaz concurrent tests
abstract class TimerSpec[F[_]: MonadAsync: Monad: Catchable: Nondeterminism] extends org.specs2.mutable.SpecificationWithJUnit {

  import MonadAsync.syntax._

  def run[A](f: F[A]): A

  def withTimer[T](expression: Timer => T): T = {
    val timer = Timer(timeoutTickMs = 10)
    try {
      expression(timer)
    } finally {
      timer.stop()
    }
  }

  "Timer" should {
    "stop normally".in {
      withTimer { _ => ok }
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
          val future = timer.valueWait[F, String]("Test", 100)
          (run(future) === "Test") and ((System.currentTimeMillis - start) >= 100)
        }
      }
    }
    "withTimeout(Future...) produces a Timeout if the timeout is exceeded" in {
      withTimer { timer =>
        val future = timer.withTimeout(async { Thread.sleep(500); "Test" }, 100)
        WithTimeout(5000) {
          run(future) must_== Timeout().left
        }
      }
    }
    "produces the result of the Future if the timeout is not exceeded" in {
      withTimer { timer =>
        val future = timer.withTimeout(async { Thread.sleep(100); "Test" }, 500)
        WithTimeout(5000) {
          run(future) must_== "Test".right
        }
      }
    }
  }
}

import MonadAsync.TaskMonadAsync
object TaskTimerSpec extends TimerSpec[Task] {
  def run[A](f: Task[A]): A = f.run
}

import ScalaFuture._
import scala.concurrent.{ Await => SAwait, Future => SFuture, duration }
import duration._
object ScalaFutureTimerSpec extends TimerSpec[SFuture] {
  def run[A](f: SFuture[A]): A = SAwait.result(f, 1000 millis)
}
