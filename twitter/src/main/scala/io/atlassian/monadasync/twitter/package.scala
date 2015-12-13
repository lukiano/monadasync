package io.atlassian.monadasync

import com.twitter.util._

import scalaz._
import scalaz.syntax.all._

package object twitter {
  implicit object FutureMonad extends MonadError[Lambda[(?, α) => Future[α]], Throwable]
      with MonadPlus[Future] with Monad[Future] with Comonad[Future]
      with Nondeterminism[Future] with Zip[Future] with Catchable[Future]
      with MonadAsync[Future] with Traverse[Future] {
    import Future._

    override def raiseError[A](e: Throwable): Future[A] =
      exception(e)

    override def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
      fa rescue { case t => f(t) }

    override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa flatMap f

    override def point[A](a: => A): Future[A] =
      delay(a)

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] =
      fa map f

    override def ap[A, B](fa: => Future[A])(f: => Future[A => B]): Future[B] =
      (f join fa) map { case (f1, a1) => f1(a1) }

    override def copoint[A](p: Future[A]): A =
      Await.result(p)

    override def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] =
      Future(f(fa))

    override def empty[A]: Future[A] =
      raiseError(new Try.PredicateDoesNotObtain)

    override def plus[A](fa: Future[A], fb: => Future[A]): Future[A] =
      fa rescue { case _ => fb }

    override def filter[A](fa: Future[A])(f: A => Boolean): Future[A] =
      fa filter f

    override def chooseAny[A](head: Future[A], tail: Seq[Future[A]]): Future[(A, Seq[Future[A]])] =
      select(head +: tail) flatMap {
        case (either, residuals) => const(either) map { (_, residuals) }
      }

    override def gather[A](fs: Seq[Future[A]]): Future[List[A]] =
      collect(fs) map { _.toList }

    override def zip[A, B](a: => Future[A], b: => Future[B]): Future[(A, B)] =
      a join b

    override def attempt[A](fa: Future[A]): Future[Throwable \/ A] =
      fa transform {
        case Return(a) => now(a.right)
        case Throw(t) => now(t.left[A])
      }

    override def fail[A](e: Throwable): Future[A] =
      raiseError(e)

    /**
     * @return an F whose value will be set from an asynchronous computation, via callback.
     */
    override def async[A](listen: Callback[A]): Future[A] =
      new Promise[A] <| { p => listen(p.setValue) }

    override implicit def monad: Monad[Future] =
      this

    override def now[A](a: A): Future[A] =
      value(a)

    override def delay[A](a: => A): Future[A] =
      value(()) map { _ => a }

    override def traverseImpl[G[_]: Applicative, A, B](fa: Future[A])(f: A => G[B]): G[Future[B]] =
      copoint(fa map f) map value
  }

  def monadState[S]: MonadState[Lambda[(?, α) => Future[α]], Option[S]] =
    new MonadState[Lambda[(?, α) => Future[α]], Option[S]] {
      val local = new Local[S]

      override def init: Future[Option[S]] =
        point(local())

      override def get: Future[Option[S]] = init

      override def put(s: Option[S]): Future[Unit] =
        point(local.set(s))

      override def modify(f: Option[S] => Option[S]): Future[Unit] =
        point(local.set(f(local())))

      override def gets[A](f: Option[S] => A): Future[A] =
        point(f(local()))

      override def point[A](a: => A): Future[A] =
        FutureMonad.point(a)

      override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        FutureMonad.bind(fa)(f)
    }

  /**
   * Future ~> F
   * Caution: The F will most likely start computing immediately
   */
  implicit class TwitterFutureAsync[A](val f: Future[A]) extends AnyVal {
    def liftAsync[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): F[A] = {
      implicit val M = MA.monad
      MA.async(f.callback).unattempt
    }
    def callback: Callback[Throwable \/ A] = { cb =>
      f.respond {
        case Return(a) => cb(\/-(a))
        case Throw(t) => cb(-\/(t))
      }
      ()
    }
  }
  implicit def twitterFutureTransformation[F[_]](implicit MA: MonadAsync[F], C: Catchable[F]): Future ~> F =
    new (Future ~> F) {
      def apply[A](f: Future[A]): F[A] = f.liftAsync[F]
    }
}
