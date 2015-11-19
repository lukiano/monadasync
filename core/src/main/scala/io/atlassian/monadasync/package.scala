package io.atlassian

import java.util.concurrent.{ Executor, ScheduledExecutorService }

import scala.concurrent.{ ExecutionContext, Future => SFuture }
import scalaz._
import scalaz.concurrent.{ Future, Strategy, Task }
import scalaz.syntax.catchable._
import scalaz.syntax.monad._
import monadasync.MonadAsync.syntax._

package object monadasync {
  // Fire asynchoronous tasks from a given ExecutorService
  type ReaderExecutorService[A] = ReaderT[Task, ScheduledExecutorService, A]

  type Callback[A] = (A => Unit) => Unit

  implicit class FutureToCallback[A](val f: Future[A]) extends AnyVal {
    def callback: Callback[A] = f.runAsync
  }
  implicit class ScalaFutureToCallback[A](val f: SFuture[A]) extends AnyVal {
    import scala.util._
    def callback(implicit ec: ExecutionContext): Callback[Throwable \/ A] = { cb =>
      f.onComplete {
        case Success(a) => cb(\/-(a))
        case Failure(t) => cb(-\/(t))
      }
    }
  }
  implicit class TaskToCallback[A](val f: Task[A]) extends AnyVal {
    def callback: Callback[Throwable \/ A] = f.runAsync
  }
  implicit class Unattempt[F[_]: Monad: Catchable, A](fa: F[Throwable \/ A]) {
    def unattempt: F[A] =
      fa >>= {
        case \/-(a)   => a.point[F]
        case -\/(err) => Catchable[F].fail(err)
      }
  }

  def attempt[F[_]: MonadAsync: Monad: Catchable, A](a: => A)(implicit pool: Executor = DefaultScheduler): F[A] =
    Monad[F].point(a).attempt.flatMap[A] {
      case \/-(v) => Monad[F].point(v)
      case -\/(t) => Catchable[F].fail(t)
    }.fork

  val DefaultExecutor = Strategy.DefaultExecutorService
  val DefaultScheduler = Strategy.DefaultTimeoutScheduler
}
