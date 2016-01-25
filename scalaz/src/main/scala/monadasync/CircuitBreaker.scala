/**
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
// licensed under the Apache 2 license
package monadasync

import java.util.concurrent.atomic.{ AtomicReference, AtomicInteger, AtomicLong, AtomicBoolean }
import scala.util.control.NoStackTrace
import java.util.concurrent.CopyOnWriteArrayList
import scala.concurrent.duration._
import scala.concurrent.TimeoutException
import scalaz.{ Nondeterminism, -\/, \/-, Catchable }
import scalaz.syntax.catchable._
import scalaz.syntax.monad._
import MonadAsync.syntax._
import scala.collection.convert.decorateAsScala._

/**
 * Provides circuit breaker functionality to provide stability when working with "dangerous" operations, e.g. calls to
 * remote systems
 *
 * Transitions through three states:
 * - In *Closed* state, calls pass through until the `maxFailures` count is reached.  This causes the circuit breaker
 * to open.  Both exceptions and calls exceeding `callTimeout` are considered failures.
 * - In *Open* state, calls fail-fast with an exception.  After `resetTimeout`, circuit breaker transitions to
 * half-open state.
 * - In *Half-Open* state, the first call will be allowed through, if it succeeds the circuit breaker will reset to
 * closed state.  If it fails, the circuit breaker will re-open to open state.  All calls beyond the first that
 * execute while the first is running will fail-fast with an exception.
 *
 * @param maxFailures Maximum number of failures before opening the circuit
 * @param callTimeout [[scala.concurrent.duration.FiniteDuration]] of time after which to consider a call a failure
 * @param resetTimeout [[scala.concurrent.duration.FiniteDuration]] of time after which to attempt to close the circuit
 */
// Based on the one from Akka, but applies to any MonadAsync
case class CircuitBreaker[F[_]: MonadAsync: Catchable: Nondeterminism](maxFailures: Int, callTimeout: FiniteDuration, resetTimeout: FiniteDuration) {

  /**
   * Holds reference to current state of CircuitBreaker - *access only via helper methods*
   */
  private[this] val state: AtomicReference[State] = new AtomicReference[State](Closed)

  /**
   * Helper method for access to underlying state via Unsafe
   *
   * @param oldState Previous state on transition
   * @param newState Next state on transition
   * @return Whether the previous state matched correctly
   */
  @inline
  private[this] def swapState(oldState: State, newState: State): Boolean =
    state.compareAndSet(oldState, newState)

  /**
   * Helper method for accessing underlying state via Unsafe
   *
   * @return Reference to current state
   */
  @inline
  private[this] def currentState: State =
    state.get()

  /**
   * Wraps invocations of asynchronous calls that need to be protected
   *
   * @param body Call needing protected
   * @return F containing the call result or a
   *   `scala.concurrent.TimeoutException` if the call timed out
   *
   */
  def withCircuitBreaker[T](body: => F[T]): F[T] =
    currentState.invoke(body)

  /**
   * Adds a callback to execute when circuit breaker opens
   *
   * The callback is run in the [[scala.concurrent.ExecutionContext]] supplied in the constructor.
   *
   * @param callback Handler to be invoked on state change
   * @return CircuitBreaker for fluent usage
   */
  def onOpen(callback: => Unit): CircuitBreaker[F] = {
    Open addListener callback
    this
  }

  /**
   * Adds a callback to execute when circuit breaker transitions to half-open
   *
   * The callback is run in the [[scala.concurrent.ExecutionContext]] supplied in the constructor.
   *
   * @param callback Handler to be invoked on state change
   * @return CircuitBreaker for fluent usage
   */
  def onHalfOpen(callback: => Unit): CircuitBreaker[F] = {
    HalfOpen addListener callback
    this
  }

  /**
   * Adds a callback to execute when circuit breaker state closes
   *
   * The callback is run in the [[scala.concurrent.ExecutionContext]] supplied in the constructor.
   *
   * @param callback Handler to be invoked on state change
   * @return CircuitBreaker for fluent usage
   */
  def onClose(callback: => Unit): CircuitBreaker[F] = {
    Closed addListener callback
    this
  }

  /**
   * Retrieves current failure count.
   *
   * @return count
   */
  private[monadasync] def currentFailureCount: Int = Closed.get

  /**
   * Implements consistent transition between states. Throws IllegalStateException if an invalid transition is attempted.
   *
   * @param fromState State being transitioning from
   * @param toState State being transitioning from
   */
  private def transition(fromState: State, toState: State): F[Unit] =
    if (swapState(fromState, toState)) {
      toState.enter()
    } else {
      // else some other thread already swapped state
      unit()
    }

  /**
   * Trips breaker to an open state.  This is valid from Closed or Half-Open states.
   *
   * @param fromState State we're coming from (Closed or Half-Open)
   */
  private def tripBreaker(fromState: State): F[Unit] =
    transition(fromState, Open)

  /**
   * Resets breaker to a closed state.  This is valid from an Half-Open state only.
   *
   */
  private def resetBreaker(): F[Unit] =
    transition(HalfOpen, Closed)

  /**
   * Attempts to reset breaker by transitioning to a half-open state.  This is valid from an Open state only.
   *
   */
  private def attemptReset(): F[Unit] =
    transition(Open, HalfOpen)

  private def unit(): F[Unit] =
    ().now[F]

  private def fail[A](t: Throwable): F[A] =
    Catchable[F].fail(t)

  private val timeout = new TimeoutException("Circuit Breaker Timed out.") with NoStackTrace

  /**
   * Internal state abstraction
   */
  private sealed trait State {
    private val listeners = new CopyOnWriteArrayList[() => Unit]

    /**
     * Add a listener function which is invoked on state entry
     *
     * @param listener listener implementation
     */
    def addListener(listener: => Unit): Unit = {
      listeners add { () => listener }
      ()
    }

    /**
     * Test for whether listeners exist
     *
     * @return whether listeners exist
     */
    private def hasListeners: Boolean = !listeners.isEmpty

    /**
     * Notifies the listeners of the transition event via a Future executed in implicit parameter ExecutionContext
     *
     * @return Promise which executes listener in supplied [[scala.concurrent.ExecutionContext]]
     */
    protected def notifyTransitionListeners(): F[Unit] =
      if (hasListeners) {
        val MAF = MonadAsync[F]
        Nondeterminism[F].gatherUnordered(listeners.asScala.map { run =>
          MAF.delay { run() }
        })
        unit()
      } else {
        unit()
      }

    /**
     * Shared implementation of call across all states.  Thrown exception or execution of the call beyond the allowed
     * call timeout is counted as a failed call, otherwise a successful call
     *
     * @param body Implementation of the call
     * @return Future containing the result of the call
     */
    def callThrough[T](body: => F[T]): F[T] =
      if (callTimeout == Duration.Zero) {
        body
      } else {
        Timer.default.withTimeout(body, callTimeout).attempt >>= {
          case -\/(t) =>
            callFails() >> fail(t)
          case \/-(-\/(t)) =>
            callFails() >> fail(timeout)
          case \/-(\/-(a)) =>
            callSucceeds() >| a
        }
      }

    /**
     * Abstract entry point for all states
     *
     * @param body Implementation of the call that needs protected
     * @return Future containing result of protected call
     */
    def invoke[T](body: => F[T]): F[T]

    /**
     * Invoked when call succeeds
     *
     */
    def callSucceeds(): F[Unit]

    /**
     * Invoked when call fails
     *
     */
    def callFails(): F[Unit]

    /**
     * Invoked on the transitioned-to state during transition.  Notifies listeners after invoking subclass template
     * method doEnter
     *
     */
    final def enter(): F[Unit] =
      doEnter() >> notifyTransitionListeners()

    /**
     * Template method for concrete traits
     *
     */
    def doEnter(): F[Unit]
  }

  /**
   * Concrete implementation of Closed state
   */
  private object Closed extends AtomicInteger with State {

    /**
     * Implementation of invoke, which simply attempts the call
     *
     * @param body Implementation of the call that needs protected
     * @return Future containing result of protected call
     */
    override def invoke[T](body: => F[T]): F[T] =
      callThrough(body)

    /**
     * On successful call, the failure count is reset to 0
     *
     * @return
     */
    override def callSucceeds(): F[Unit] =
      set(0).now[F]

    /**
     * On failed call, the failure count is incremented.  The count is checked against the configured maxFailures, and
     * the breaker is tripped if we have reached maxFailures.
     *
     * @return
     */
    override def callFails(): F[Unit] =
      if (incrementAndGet() == maxFailures) {
        tripBreaker(Closed)
      } else {
        unit()
      }

    /**
     * On entry of this state, failure count is reset.
     *
     * @return
     */
    override def doEnter(): F[Unit] =
      set(0).now[F]

    /**
     * Override for more descriptive toString
     *
     * @return
     */
    override def toString: String =
      "Closed with failure count = " + get()
  }

  /**
   * Concrete implementation of half-open state
   */
  private object HalfOpen extends AtomicBoolean(true) with State {

    /**
     * Allows a single call through, during which all other callers fail-fast.  If the call fails, the breaker reopens.
     * If the call succeeds the breaker closes.
     *
     * @param body Implementation of the call that needs protected
     * @return Future containing result of protected call
     */
    override def invoke[T](body: => F[T]): F[T] =
      if (compareAndSet(true, false)) {
        callThrough(body)
      } else {
        fail(new CircuitBreakerOpenException(0.seconds))
      }

    /**
     * Reset breaker on successful call.
     *
     * @return
     */
    override def callSucceeds(): F[Unit] =
      resetBreaker()

    /**
     * Reopen breaker on failed call.
     *
     * @return
     */
    override def callFails(): F[Unit] =
      tripBreaker(HalfOpen)

    /**
     * On entry, guard should be reset for that first call to get in
     *
     * @return
     */
    override def doEnter(): F[Unit] =
      set(true).now[F]

    /**
     * Override for more descriptive toString
     *
     * @return
     */
    override def toString: String =
      "Half-Open currently testing call for success = " + get()
  }

  /**
   * Concrete implementation of Open state
   */
  private object Open extends AtomicLong with State {

    /**
     * Fail-fast on any invocation
     *
     * @param body Implementation of the call that needs protected
     * @return Future containing result of protected call
     */
    override def invoke[T](body: => F[T]): F[T] =
      fail(new CircuitBreakerOpenException(remainingDuration()))

    /**
     * Calculate remaining duration until reset to inform the caller in case a backoff algorithm is useful
     *
     * @return duration to when the breaker will attempt a reset by transitioning to half-open
     */
    private def remainingDuration(): FiniteDuration = {
      val diff = System.nanoTime() - get
      if (diff <= 0L) {
        Duration.Zero
      } else {
        diff.nanos
      }
    }

    /**
     * No-op for open, calls are never executed so cannot succeed or fail
     *
     * @return
     */
    override def callSucceeds(): F[Unit] =
      unit()

    /**
     * No-op for open, calls are never executed so cannot succeed or fail
     *
     * @return
     */
    override def callFails(): F[Unit] =
      unit()

    /**
     * On entering this state, schedule an attempted reset and store the entry time to
     * calculate remaining time before attempted reset.
     *
     * @return
     */
    override def doEnter(): F[Unit] = {
      set(System.nanoTime())
      Timer.default.valueWait((), resetTimeout.toMillis) >> { attemptReset() }
      unit()
    }

    /**
     * Override for more descriptive toString
     *
     * @return
     */
    override def toString: String =
      "Open"
  }
}

/**
 * Exception thrown when Circuit Breaker is open.
 *
 * @param remainingDuration Stores remaining time before attempting a reset.  Zero duration means the breaker is
 *                          currently in half-open state.
 * @param message Defaults to "Circuit Breaker is open; calls are failing fast"
 */
class CircuitBreakerOpenException(
  val remainingDuration: FiniteDuration,
  message: String = "Circuit Breaker is open; calls are failing fast"
) extends RuntimeException(message) with NoStackTrace