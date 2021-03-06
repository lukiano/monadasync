package monadasync

import java.util.concurrent.Executor

import cats._
import free.Trampoline
import std.function.function0Instance
import scala.annotation.tailrec
import scala.concurrent.duration
import Trampoline.done

// Until Cats gets their own
sealed trait Future[+A] {
  import Future._

  @tailrec
  private def step: DirectFuture[A] = this match {
    case n @ Now(a) => n
    case Suspend(thunk) => thunk.run.step
    case a @ Async(onFinish, f) => a
  }

  private[Future] def listen(effect: A => Unit): Unit = this.step match {
    case Now(a) => effect(a)
    case as @ Async(onFinish, f) => onFinish {
      a =>
        for {
          ta <- done(a)
          fta <- f(ta)
        } yield fta listen effect
    }
  }

  /** Run this computation to obtain an `A`, then invoke the given callback. */
  final val runAsync: Callback[A] = listen
}

object Future {
  sealed trait DirectFuture[+A] extends Future[A]

  private case class Now[A](a: A) extends DirectFuture[A]
  private case class Suspend[A](thunk: Trampoline[Future[A]]) extends Future[A]
  private case class Async[A, B](onFinish: (A => Trampoline[Unit]) => Unit, f: A => Trampoline[Future[B]]) extends DirectFuture[B]

  implicit object instance extends Monad[Future] with MonadAsync[Future] {
    def pure[A](a: A): Future[A] = now(a)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = {
      def loop(thunk: Future[A]): Future[B] = flatMap(thunk)(f)
      fa match {
        case Now(a) => Suspend(done(a) map f)
        case Suspend(thunk) => Suspend(thunk map loop)
        case Async(onFinish, g) => Async(onFinish, (a: Any) => g(a) map loop)
      }
    }
    def now[A](a: A): Future[A] = Now(a)
    def async[A](listen: Callback[A]): Future[A] = Async[A, A](cb => listen { a => cb(a).run }, a => done(now(a)))
    def delay[A](a: => A): Future[A] = suspend(now(a))
    def suspend[A](f: => Future[A]): Future[A] = Suspend(Trampoline.delay(f))
    def async[A](pool: Executor)(a: => A): Future[A] =
      async({ cb: (A => Unit) =>
        pool.execute(new Runnable { def run() = cb(a) })
      })
  }

  implicit class FutureOps[A](val fa: Future[A]) extends AnyVal {
    import syntax.flatMap._

    /** Returns a `Future` that delays the execution of this `Future` by the duration `t`. */
    def after(t: duration.Duration): Future[A] = after(t.toMillis)
    def after(t: Long): Future[A] = Timer.default.valueWait[Future, Unit]((), t) >> fa
  }
}