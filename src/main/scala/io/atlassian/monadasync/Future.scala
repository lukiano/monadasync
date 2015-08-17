package io.atlassian.monadasync

import java.util.concurrent.atomic.AtomicReference

import cats.Monad
import cats.free.Trampoline
import cats.std.function._
import cats.syntax.flatMap._

import scala.concurrent.duration.Duration

sealed abstract class Future[+A] {
  import Future._

  @annotation.tailrec
  private final def step: Future[A] = this match {
    case Suspend(thunk)        => thunk().step
    case BindSuspend(thunk, f) => (thunk() flatMap f).step
    case _                     => this
  }

  /**
   * Run this computation to obtain an `A`, then invoke the given callback.
   * Also see `runAsync`.
   */
  def listen(cb: A => Trampoline[Unit]): Unit =
    this.step match {
      case Now(a)          => cb(a).run
      case Async(onFinish) => onFinish(cb)
      case BindAsync(onFinish, g) =>
        onFinish(x => Trampoline.delay(g(x)) map (_ listen cb))
      case err => sys.error("Step returned an invalid state " + err)
    }

  def runAsync(cb: A => Unit): Unit =
    listen(a => Trampoline.done(cb(a)))

  /** Run this `Future` and block awaiting its result. */
  def run: A = this match {
    case Now(a) => a
    case _ =>
      val latch = new java.util.concurrent.CountDownLatch(1)
      val result = new AtomicReference[A]
      runAsync { a => result.set(a); latch.countDown() }
      latch.await()
      result.get
  }

  /**
   * Returns a `Future` that delays the execution of this `Future` by the duration `t`.
   */
  def after(t: Duration): Future[A] =
    after(t.toMillis)

  def after(t: Long): Future[A] =
    Timer.default.valueWait[Future, Unit]((), t) >> this

}

object Future {
  case class Now[+A](a: A) extends Future[A]
  case class Async[+A](onFinish: (A => Trampoline[Unit]) => Unit) extends Future[A]
  case class Suspend[+A](thunk: () => Future[A]) extends Future[A]
  case class BindSuspend[A, B](thunk: () => Future[A], f: A => Future[B]) extends Future[B]
  case class BindAsync[A, B](onFinish: (A => Trampoline[Unit]) => Unit,
                             f: A => Future[B]) extends Future[B]

  implicit val Monad: Monad[Future] = new Monad[Future] {
    override def pure[A](x: A): Future[A] = Now(x)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa match {
        case Now(a)         => Suspend(() => f(a))
        case Suspend(thunk) => BindSuspend(thunk, f)
        case Async(listen)  => BindAsync(listen, f)
        case BindSuspend(thunk, g) =>
          Suspend(() => BindSuspend(thunk, g andThen (_ flatMap f)))
        case BindAsync(listen, g) =>
          Suspend(() => BindAsync(listen, g andThen (_ flatMap f)))
      }
  }

  def async[A](listen: (A => Unit) => Unit): Future[A] =
    Async((cb: A => Trampoline[Unit]) => listen { a => cb(a).run })

  def delay[A](a: => A): Future[A] = Suspend(() => Now(a))

  /**
   * Produce `f` in the main trampolining loop, `Future.step`, using a fresh
   * call stack. The standard trampolining primitive, useful for avoiding
   * stack overflows.
   */
  def suspend[A](f: => Future[A]): Future[A] = Suspend(() => f)
}
