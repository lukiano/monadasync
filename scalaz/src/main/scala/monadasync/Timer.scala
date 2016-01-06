package monadasync

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.{ Lock, ReentrantReadWriteLock }

import MonadAsync.syntax._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.Duration
import scalaz.syntax.monad._
import scalaz.{ Nondeterminism, \/ }

// Same as scalaz.concurrent.Timer, but applies to any MonadAsync
case class Timer(timeoutTickMs: Long = Timer.defaultTimeoutMs, workerName: String = "TimeoutContextWorker") {
  val safeTickMs = if (timeoutTickMs > Timer.minTickMs) timeoutTickMs else Timer.minTickMs
  type Instant = Long
  type FuturesMap = SortedMap[Instant, List[() => Unit]]
  private[this] val continueRunning = new AtomicBoolean(true)
  @volatile private[this] var lastNow: Instant = alignTimeResolution(System.currentTimeMillis)
  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var futures: FuturesMap = SortedMap()
  private[this] val workerRunnable = new Runnable() {
    def run() {
      @tailrec
      def innerRun() {
        lastNow = alignTimeResolution(System.currentTimeMillis)
        // Deal with stuff to expire.
        futures.headOption match {
          case Some((time, _)) if time <= lastNow =>
            val expiredFutures: FuturesMap = withWrite {
              val (past, future) = futures.span { case (instant, _) => instant < lastNow }
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

  private[this] def expireFutures(futures: FuturesMap) =
    futures.foreach { case (_, list) => list.foreach(call => call()) }

  def stop(expireImmediately: Boolean = false): Unit =
    withWrite {
      continueRunning.set(false)
      if (expireImmediately) {
        expireFutures(futures)
        futures = SortedMap()
      }
    }

  private[this] def withWrite[A](expression: => A): A =
    withLock(lock.writeLock(), expression)

  private[this] def withRead[A](expression: => A): A =
    withLock(lock.readLock(), expression)

  private[this] def withLock[A](l: Lock, expression: => A): A = {
    l.lock()
    try {
      expression
    } finally {
      l.unlock()
    }
  }

  private[this] def alignTimeResolution(time: Instant): Instant = time / timeoutTickMs * timeoutTickMs

  def valueWait[F[_]: MonadAsync, A](value: A, waitMs: Long): F[A] =
    withRead {
      if (continueRunning.get()) {
        val listen: Callback[A] = callback => withWrite {
          val waitTime = alignTimeResolution(lastNow + (if (waitMs < 0) 0 else waitMs))
          val timedCallback = () => callback(value)
          // Lazy implementation for now.
          val newEntry: (Instant, List[() => Unit]) = futures.get(waitTime) match {
            case None => waitTime -> List(timedCallback)
            case Some(list) => waitTime -> (timedCallback :: list)
          }
          futures = futures + newEntry
        }
        listen.liftAsync[F]
      } else {
        value.now[F]
      }
    }

  def withTimeout[F[_]: MonadAsync: Nondeterminism, A](f: F[A], timeout: Long): F[Timeout \/ A] =
    Nondeterminism[F].choose(valueWait(Timeout(), timeout), f) map (_.bimap(_._1, _._2))

  def withTimeout[F[_]: MonadAsync: Nondeterminism, A](f: F[A], timeout: Duration): F[Timeout \/ A] =
    withTimeout(f, timeout.toMillis)
}

object Timer {
  lazy val default = Timer()
  val minTickMs = 5L
  val defaultTimeoutMs = 100L
}