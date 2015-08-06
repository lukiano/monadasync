package io.atlassian.monadasync

import java.util.concurrent.{ CountDownLatch, TimeUnit }

final class WithTimeout(timeout: Long) {
  def apply[A](test: => A): A = {
    val latch = new CountDownLatch(1)
    @volatile var result: A = null.asInstanceOf[A]
    fork {
      result = test
      latch.countDown()
    }
    if (latch.await(timeout, TimeUnit.MILLISECONDS)) result
    else sys.error("Timeout occured, possible deadlock.")
  }

  private def fork(f: => Unit) {
    new Thread {
      override def run() {
        f
      }
    }.start()
  }
}

object WithTimeout {
  def apply(timeout: Long) = new WithTimeout(timeout)
}
