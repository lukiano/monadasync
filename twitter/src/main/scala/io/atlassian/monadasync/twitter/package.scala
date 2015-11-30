package io.atlassian.monadasync

import com.twitter.util._

import scalaz._
import scalaz.syntax.either._

package object twitter {

  implicit object FutureMonadAsync extends MonadAsync[Future] {
    /**
     * @return an F whose value will be set from an asynchronous computation, via callback.
     */
    override def async[A](listen: Callback[A]): Future[A] = {
      val p = new Promise[A]
      listen(p.setValue)
      p
    }

    /**
     * @return the underlying monad.
     */
    override implicit def monad: Monad[Future] =
      FutureMonad

    /**
     * @return an F whose value is immediately set.
     */
    override def now[A](a: A): Future[A] =
      Future.value(a)

    /**
     * @return an F whose value will be computed when called, on the caller thread.
     */
    override def delay[A](a: => A): Future[A] =
      Future(a)
  }

  implicit object FutureCatchable extends Catchable[Future] {
    override def attempt[A](fa: Future[A]): Future[Throwable \/ A] =
      fa transform {
        case Return(a) => Future.value(a.right)
        case Throw(t) => Future.value(t.left[A])
      }

    override def fail[A](e: Throwable): Future[A] =
      Future.rawException(e)
  }

  implicit object FutureMonad extends MonadError[Lambda[(?, A) => Future[A]], Throwable]
      with MonadPlus[Future] with Monad[Future] with Comonad[Future] with Nondeterminism[Future] {

    override def raiseError[A](e: Throwable): Future[A] =
      Future.exception(e)

    override def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
      fa rescue { case t => f(t) }

    override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa flatMap f

    override def point[A](a: => A): Future[A] =
      Future(a)

    override def copoint[A](p: Future[A]): A =
      Await.result(p)

    override def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] =
      Future(f(fa))

    override def empty[A]: Future[A] =
      Future.exception(new Try.PredicateDoesNotObtain)

    override def plus[A](a: Future[A], b: => Future[A]): Future[A] =
      Futures.join(a.liftToTry, b.liftToTry) flatMap {
        case (Return(va), Return(_)) => Future.value(va)
        case (Return(va), Throw(_)) => Future.value(va)
        case (Throw(_), Return(vb)) => Future.value(vb)
        case (Throw(ta), Throw(_)) => Future.exception(ta)
      }

    override def chooseAny[A](head: Future[A], tail: Seq[Future[A]]): Future[(A, Seq[Future[A]])] =
      Future.select(head +: tail) flatMap {
        case (either, residuals) => Future.const(either) map { (_, residuals) }
      }
  }
}
