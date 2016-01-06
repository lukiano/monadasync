package monadasync
import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._

object Catchables {

  implicit def CatchableEitherT[F[_]: Monad]: Catchable[EitherT[F, Throwable, ?]] = new Catchable[EitherT[F, Throwable, ?]] {
    override def attempt[A](f: EitherT[F, Throwable, A]): EitherT[F, Throwable, Throwable \/ A] =
      EitherT[F, Throwable, Throwable \/ A](f.run map { _.right[Throwable] })

    override def fail[A](err: Throwable): EitherT[F, Throwable, A] =
      EitherT[F, Throwable, A](err.left[A].point[F])
  }

  implicit def catchableWriterT[G[_], W](implicit C: Catchable[G], M: Functor[G], W: Monoid[W]): Catchable[WriterT[G, W, ?]] = new Catchable[WriterT[G, W, ?]] {
    override def attempt[A](f: WriterT[G, W, A]): WriterT[G, W, Throwable \/ A] =
      WriterT[G, W, Throwable \/ A] {
        C.attempt(f.run) ∘ {
          case \/-((w, a)) => (w, \/-(a))
          case -\/(t) => (∅[W], -\/(t))
        }
      }
    override def fail[A](err: Throwable): WriterT[G, W, A] =
      WriterT[G, W, A](C.fail(err))
  }

}
