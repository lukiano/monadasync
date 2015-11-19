package io.atlassian.monadasync

import java.util.concurrent.RejectedExecutionException

import scalaz.Dequeue
import scalaz.syntax.catchable._
import scalaz.syntax.monad._
import MonadAsync.syntax._

import scalaz.{ -\/, \/-, Catchable }

// based on the one from Twitter-Util, adapted to any F
class AsyncSemaphore[F[_]: MonadAsync: Catchable] protected (initialPermits: Int, maxWaiters: Option[Int]) {
  implicit val monad = MonadAsync[F].monad

  /**
   * Constructs a semaphore with no limit on the max number
   * of waiters for permits.
   *
   * @param initialPermits must be positive
   */
  def this(initialPermits: Int) = this(initialPermits, None)

  /**
   * Constructs a semaphore with `maxWaiters` as the limit on the
   * number of waiters for permits.
   *
   * @param initialPermits must be positive
   * @param maxWaiters must be non-negative
   */
  def this(initialPermits: Int, maxWaiters: Int) = this(initialPermits, Some(maxWaiters))

  require(maxWaiters.getOrElse(0) >= 0, s"maxWaiters must be non-negative: $maxWaiters")
  require(initialPermits > 0, s"initialPermits must be positive: $initialPermits")

  private[this] val waitq = new java.util.ArrayDeque[Permit => Unit]
  private[this] var availablePermits = initialPermits

  final class SemaphorePermit extends Permit {
    /**
     * Indicate that you are done with your Permit.
     */
    def release(): Unit = {
      AsyncSemaphore.this.synchronized {
        Option(waitq.pollFirst()) match {
          case Some(next) => next(new SemaphorePermit)
          case None => availablePermits += 1
        }
      }
    }
  }

  def numWaiters: Int = synchronized(waitq.size)
  def numInitialPermits: Int = initialPermits
  def numPermitsAvailable: Int = synchronized(availablePermits)

  /**
   * Acquire a Permit, asynchronously. Be sure to permit.release() in a 'finally'
   * block of your onSuccess() callback.
   *
   * Interrupting this future is only advisory, and will not release the permit
   * if the future has already been satisfied.
   *
   * @return a Future[Permit] when the Future is satisfied, computation can proceed,
   * or a Future.Exception[RejectedExecutionException] if the configured maximum number of waitq
   * would be exceeded.
   */
  def acquire(): F[Permit] = {
    synchronized {
      if (availablePermits > 0) {
        availablePermits -= 1
        MonadAsync[F].now(new SemaphorePermit)
      } else {
        maxWaiters match {
          case Some(max) if waitq.size >= max =>
            Catchable[F].fail(new RejectedExecutionException("Max waiters exceeded"))
          case _ =>
            MonadAsync[F].async[Permit](waitq.addLast _)
        }
      }
    }
  }

  /**
   * Execute the function asynchronously when a permit becomes available.
   *
   * If the function throws a non-fatal exception, the exception is returned as part of the Future.
   * For all exceptions, the permit would be released before returning.
   *
   * @return a Future[T] equivalent to the return value of the input function. If the configured
   *         maximum value of waitq is reached, Future.Exception[RejectedExecutionException] is
   *         returned.
   */
  def acquireAndRun[T](func: => F[T]): F[T] =
    acquire() flatMap { permit =>
      try {
        func.attempt flatMap {
          case \/-(a) =>
            permit.release()
            a.now
          case -\/(e) =>
            permit.release()
            Catchable[F].fail(e)
        }
      } catch {
        case scala.util.control.NonFatal(e) =>
          permit.release()
          Catchable[F].fail(e)
        case e: Throwable =>
          permit.release()
          throw e
      }
    }

  /**
   * Execute the function when a permit becomes available.
   *
   * If the function throws an exception, the exception is returned as part of the Future.
   * For all exceptions, the permit would be released before returning.
   *
   * @return a Future[T] equivalent to the return value of the input function. If the configured
   *         maximum value of waitq is reached, Future.Exception[RejectedExecutionException] is
   *         returned.
   */
  def acquireAndRunSync[T](func: => T): F[T] =
    acquire() flatMap { permit =>
      try {
        val t = func
        permit.release()
        t.now
      } catch {

        case e: Throwable =>
          permit.release()
          Catchable[F].fail(e)
      }
    }
}

class AsyncMutex[F[_]: MonadAsync: Catchable] protected (maxWaiters: Option[Int]) extends AsyncSemaphore[F](1, maxWaiters) {
  /**
   * Constructs a mutex with no limit on the max number
   * of waiters for permits.
   */
  def this() = this(None)

  /**
   * Constructs a mutex with `maxWaiters` as the limit on the
   * number of waiters for permits.
   */
  def this(maxWaiters: Int) = this(Some(maxWaiters))
}

trait Permit {
  def release(): Unit
}

