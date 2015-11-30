package io.atlassian.monadasync

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Promise, Future, ExecutionContext }
import scala.util.Try
import scalaz._

object ScalaFuture {

  private val context: ExecutionContext = ExecutionContext.fromExecutor(MonadAsync.SameThreadExecutor)

  implicit val ScalaFutureMonadAsync: MonadAsync[Future] = scalaFutureMonadAsync(context)

  def scalaFutureMonadAsync(context: ExecutionContext): MonadAsync[Future] = new MonadAsync[Future] {
    override val monad = scalaz.std.scalaFuture.futureInstance(context)

    override def delay[A](a: => A) =
      Future(a)(context)
    override def now[A](a: A) = Future.successful(a)
    override def async[A](listen: Callback[A]) = {
      val p = Promise[A]()
      listen { a =>
        p.success(a)
        ()
      }
      p.future
    }
  }

  implicit val ScalaFutureCatchable: Catchable[Future] = scalaFutureCatchable(context)

  def scalaFutureCatchable(context: ExecutionContext): Catchable[Future] = new Catchable[Future] {
    override def attempt[A](f: Future[A]): Future[Throwable \/ A] =
      f.map({ a => \/-(a) })(context).recover({ case t => -\/(t) })(context)
    override def fail[A](err: Throwable): Future[A] =
      Future.failed[A](err)
  }

  implicit val ScalaFutureMonad = scalaFutureMonad(context)

  def scalaFutureMonad(context: ExecutionContext): MonadError[Lambda[(?, A) => Future[A]], Throwable] with Monad[Future] with Comonad[Future] with Nondeterminism[Future] =
    new MonadError[Lambda[(?, A) => Future[A]], Throwable] with Monad[Future] with Comonad[Future] with Nondeterminism[Future] {
      private val monad = scalaz.std.scalaFuture.futureInstance(context)

      override def raiseError[A](e: Throwable): Future[A] =
        Future.failed(e)

      override def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
        fa.recoverWith({ case e => f(e) })(context)

      override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        monad.bind(fa)(f)

      override def point[A](a: => A): Future[A] =
        monad.point(a)

      override def copoint[A](p: Future[A]): A =
        Await.result(p, Duration.Inf)

      override def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] =
        monad.cobind(fa)(f)

      override def chooseAny[A](head: Future[A], tail: Seq[Future[A]]): Future[(A, Seq[Future[A]])] = {
        val p = Promise[(Try[A], Future[A])]()
        val result = Promise[(A, Seq[Future[A]])]()
        val all: Seq[Future[A]] = head +: tail
        all foreach { f =>
          f.onComplete(t => p.trySuccess((t, f)))(context)
        }
        p.future.onSuccess {
          case (tryValue, futureItself) => result.complete(tryValue.map(v => (v, all.filter(_ != futureItself))))
        }(context)
        result.future
      }

    }

  /**
   * scala.Future ~> F
   * Caution: The F will most likely start computing immediately
   */
  implicit class ScalaFutureAsync[A](val f: Future[A]) extends AnyVal {
    def liftAsync[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): F[A] = {
      implicit val M = MA.monad
      MA.async(f.callback(context)).unattempt
    }
  }
  implicit def ScalaFutureTransformation[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): Future ~> F =
    new (Future ~> F) {
      def apply[A](f: Future[A]): F[A] = f.liftAsync[F]
    }

}
