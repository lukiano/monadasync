package io.atlassian.monadasync

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.ReentrantReadWriteLock

import io.atlassian.monadasync.MonadAsync.syntax._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.{ Nondeterminism, \/ }

// Same as scalaz.concurrent.Timer, but generic to any MonadAsync
case class Timer(timeoutTickMs: Long = 100, workerName: String = "TimeoutContextWorker") {
  val safeTickMs = if (timeoutTickMs > 5) timeoutTickMs else 5
  private[this] val continueRunning = new AtomicBoolean(true)
  @volatile private[this] var lastNow: Long = alignTimeResolution(System.currentTimeMillis)
  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var futures: SortedMap[Long, List[() => Unit]] = SortedMap()
  private[this] val workerRunnable = new Runnable() {
    def run() {
      @tailrec
      def innerRun() {
        lastNow = alignTimeResolution(System.currentTimeMillis)
        // Deal with stuff to expire.
        futures.headOption match {
          case Some((time, _)) if time <= lastNow =>
            val expiredFutures: SortedMap[Long, List[() => Unit]] = withWrite {
              val (past, future) = futures.span(pair => pair._1 < lastNow)
              futures = future
              past
            }
            expireFutures(expiredFutures)

          case _ => ()
        }
        // Should we keep running?
        if (continueRunning.get()) {
          Thread.sleep(safeTickMs)
          innerRun()
        }
      }
      innerRun()
    }
  }
  private[this] val workerThread = new Thread(workerRunnable, workerName)
  workerThread.start()

  private[this] def expireFutures(futures: SortedMap[Long, List[() => Unit]]) {
    futures.foreach(vector => vector._2.foreach(call => call()))
  }

  def stop(expireImmediately: Boolean = false) {
    withWrite {
      continueRunning.set(false)
      if (expireImmediately) {
        expireFutures(futures)
        futures = SortedMap()
      }
    }
  }

  private[this] def withWrite[T](expression: => T): T = {
    lock.writeLock().lock()
    try {
      expression
    } finally {
      lock.writeLock().unlock()
    }
  }

  private[this] def withRead[T](expression: => T): T = {
    lock.readLock().lock()
    try {
      expression
    } finally {
      lock.readLock().unlock()
    }
  }

  private[this] def alignTimeResolution(time: Long): Long = time / timeoutTickMs * timeoutTickMs

  def valueWait[F[_]: MonadAsync, T](value: T, waitMs: Long): F[T] = {
    withRead {
      if (continueRunning.get()) {
        val listen: Callback[T] = callback => withWrite {
          val waitTime = alignTimeResolution(lastNow + (if (waitMs < 0) 0 else waitMs))
          val timedCallback = () => callback(value)
          // Lazy implementation for now.
          futures = futures + futures.get(waitTime).map(current => (waitTime, timedCallback :: current)).getOrElse((waitTime, List(timedCallback)))
        }
        listen.liftAsync[F]
      } else {
        value.now[F]
      }
    }
  }

  def withTimeout[F[_]: MonadAsync: Nondeterminism, T](f: F[T], timeout: Long): F[Timeout \/ T] = {
    Nondeterminism[F].choose(valueWait(Timeout(), timeout), f) map (_.fold(_._1.left, _._2.right))
  }
}

object Timer {
  lazy val default = Timer()
}