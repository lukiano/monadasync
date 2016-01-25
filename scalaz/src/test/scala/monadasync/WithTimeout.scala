package monadasync
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import scala.concurrent.duration.Duration

final class WithTimeout(timeout: Long) {
  def apply[A](test: => A): A = {
    val latch = new CountDownLatch(1)
    val result = new AtomicReference[A]
    val t = fork {
      result.set(test)
      latch.countDown()
    }
    if (latch.await(timeout, TimeUnit.MILLISECONDS)) {
      result.get()
    } else {
      t.interrupt()
      sys.error("Timeout occured, possible deadlock.")
    }
  }

  private def fork(f: => Unit): Thread = {
    val t = new Thread {
      override def run() {
        f
      }
    }
    t.setDaemon(true)
    t.start()
    t
  }
}

object WithTimeout {
  def apply(timeout: Long): WithTimeout = new WithTimeout(timeout)
  def apply(timeout: Duration): WithTimeout = apply(timeout.toMillis)
}
