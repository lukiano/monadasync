package io.atlassian.monadasync

import cats._
import free.Trampoline
import scala.concurrent._

sealed trait Future[A] {
  protected def listen(cb: Future.Trampolined[A]): Unit

  /** Run this computation to obtain an `A`, then invoke the given callback. */
  final val runAsync: Callback[A] = cb => listen(a => Trampoline.done(cb(a)))

  /** Run this `Future` and block awaiting its result. */
  def run: A
}

object Future {
  import std.function._
  type Trampolined[A] = A => Trampoline[Unit]

  private case class Now[A](a: A) extends Future[A] {
    protected def listen(cb: Trampolined[A]) = cb(a).run
    def run = a
  }
  private case class Suspend[A](thunk: () => Future[A]) extends Future[A] {
    protected def listen(cb: Trampolined[A]) = thunk().listen(cb)
    def run = thunk().run
  }
  private case class Async[A, B](onFinish: Trampolined[A] => Unit, f: A => Future[B]) extends Future[B] {
    private def effect(g: Future[B] => Unit) = onFinish { a => Trampoline.delay(f(a)) map g }
    protected def listen(cb: Trampolined[B]) = effect(_ listen cb)
    def run = {
      val sync = new SyncVar[B]
      effect(_ runAsync sync.put)
      sync.take()
    }
  }

  implicit object instance extends Monad[Future] with Comonad[Future] {
    def extract[A](fa: Future[A]): A = fa.run
    def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = delay(f(fa))
    def pure[A](a: A): Future[A] = now(a)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = suspend {
      def loop(thunk: Future[A]): Future[B] = flatMap(thunk)(f)
      fa match {
        case Now(a)             => f(a)
        case Suspend(thunk)     => loop(thunk())
        case Async(onFinish, g) => Async(onFinish, g andThen loop)
      }
    }
  }

  def now[A](a: A): Future[A] = Now(a)
  def async[A](listen: Callback[A]): Future[A] = Async((cb: Trampolined[A]) => listen { a => cb(a).run }, now)
  def delay[A](a: => A): Future[A] = suspend(now(a))
  def suspend[A](f: => Future[A]): Future[A] = Suspend(() => f)

  implicit class FutureOps[A](val fa: Future[A]) extends AnyVal {
    import syntax.flatMap._

    /** Returns a `Future` that delays the execution of this `Future` by the duration `t`. */
    def after(t: duration.Duration): Future[A] = after(t.toMillis)
    def after(t: Long): Future[A] = Timer.default.valueWait[Future, Unit]((), t) >> fa
  }
}
