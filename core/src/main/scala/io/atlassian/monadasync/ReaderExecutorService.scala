package io.atlassian.monadasync

import java.util.concurrent.{ Executor, ScheduledExecutorService }

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent.Task
import scalaz.syntax.either._

/**
 * Fire asynchoronous tasks from a given ExecutorService
 */
object ReaderExecutorService {
  def apply[A](run: ScheduledExecutorService => Task[A]) = Kleisli.kleisli(run)

  implicit object ReaderExecutorServiceMonadAsync extends MonadAsync[ReaderExecutorService] {
    def monad = Monad[ReaderExecutorService]
    def delay[A](a: => A) =
      Kleisli { _ => Task.delay(a) }
    def now[A](a: A) =
      Kleisli { _ => Task.now(a) }
    override def async[A](a: => A)(implicit ignored: Executor = null) =
      Kleisli { es => Task(a)(es) }
    def async[A](listen: (A => Unit) => Unit) =
      Kleisli { _ => Task.async { callback => listen { a => callback(a.right) } } }
    override def schedule[A](a: => A, delay: Duration)(implicit ignored: ScheduledExecutorService = null) =
      Kleisli { es => Task.schedule(a, delay)(es) }
  }

  implicit object ReaderExecutorServiceMonadError extends MonadError[λ[(α, β) => ReaderExecutorService[β]], Throwable] {
    private def T = MonadError[({ type λ[α, β] = Task[β] })#λ, Throwable]
    def raiseError[A](e: Throwable): ReaderExecutorService[A] =
      Kleisli { _ => T.raiseError(e) }
    def handleError[A](fa: ReaderExecutorService[A])(f: Throwable => ReaderExecutorService[A]) =
      Kleisli { es => T.handleError(fa.run(es)) { t => f(t).run(es) } }
    def bind[A, B](fa: ReaderExecutorService[A])(f: A => ReaderExecutorService[B]) =
      Kleisli { es => fa.run(es) flatMap { a => f(a).run(es) } }
    def point[A](a: => A): ReaderExecutorService[A] =
      Kleisli { _ => Monad[Task].point(a) }
  }

  implicit object ReaderExecutorServiceCatchable extends Catchable[ReaderExecutorService] {
    def attempt[A](f: ReaderExecutorService[A]) =
      Kleisli { es => Catchable[Task].attempt(f.run(es)) }
    def fail[A](err: Throwable) =
      Kleisli { _ => Catchable[Task].fail(err) }
  }
}
