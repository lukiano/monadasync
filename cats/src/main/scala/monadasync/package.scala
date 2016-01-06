import cats.Monad
import cats.data.{ Xor, XorT }
import cats.syntax.all._

package object monadasync {
  type Callback[A] = (A => Unit) => Unit

  type Catchable[F[_]] = cats.MonadError[F, Throwable]

  def catchable[F[_]: Catchable]: Catchable[F] =
    implicitly[Catchable[F]]
  def fail[F[_]: Catchable, A](t: Throwable): F[A] =
    implicitly[Catchable[F]].raiseError[A](t)

  implicit class FutureToCallback[A](val f: Future[A]) extends AnyVal {
    def callback: Callback[A] = f.runAsync
  }
  implicit class Unattempt[F[_]: Monad: Catchable, A](fa: F[Throwable Xor A]) {
    def unattempt: F[A] =
      fa >>= {
        case Xor.Right(a) => Monad[F].pure(a)
        case Xor.Left(err) => fail[F, A](err)
      }
  }

  type Task[A] = XorT[Future, Throwable, A]
}
