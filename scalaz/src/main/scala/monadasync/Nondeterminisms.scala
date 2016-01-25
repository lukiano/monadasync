package monadasync

import scalaz._
import scalaz.syntax.monad._
import scalaz.std.tuple._

object Nondeterminisms {

  abstract class ND[G[_]] { // to avoid an infinite loop because Nondeterminism extends Monad
    def point[A](a: => A): G[A]
    def bind[A, B](ga: G[A])(f: A => G[B]): G[B]
    def chooseAny[A](head: G[A], tail: Seq[G[A]]): G[(A, Seq[G[A]])]
  }

  sealed abstract class NonDeterminismMonad[G[_]: Monad, F[_]: Nondeterminism, Inner[_]: Functor, Arg[_], X] extends ND[G] {
    def point[A](a: => A): G[A] =
      Monad[G].point(a)
    def bind[A, B](ga: G[A])(f: A => G[B]): G[B] =
      ga >>= f
    def chooseAny[A](head: G[A], tail: Seq[G[A]]): G[(A, Seq[G[A]])] =
      cons(conv { x =>
        Nondeterminism[F].chooseAny(run(head)(x), tail map { a => run(a)(x) }) map {
          case (inner, seq) => inner ∘ { a => (a, seq map { elem => cons(conv(_ => elem)) }) }
        }
      })
    protected def cons[A](fa: Arg[F[Inner[A]]]): G[A]
    protected def run[A](ga: G[A])(x: X): F[Inner[A]]
    protected def conv[A](fa: X => F[Inner[A]]): Arg[F[Inner[A]]]
  }

  implicit def eitherTNonDeterminism[F[_]: Nondeterminism, L]: ND[EitherT[F, L, ?]] =
    new NonDeterminismMonad[EitherT[F, L, ?], F, L \/ ?, Function0, Null] {
      protected def cons[A](fa: () => F[L \/ A]) = EitherT(fa())
      protected def run[A](ga: EitherT[F, L, A])(n: Null) = ga.run
      protected def conv[A](fa: Null => F[(L \/ A)]) = () => fa(null)
    }

  implicit def writerTNonDeterminism[F[_]: Nondeterminism, W: Monoid]: ND[WriterT[F, W, ?]] =
    new NonDeterminismMonad[WriterT[F, W, ?], F, (W, ?), Function0, Null] {
      protected def cons[A](fa: () => F[(W, A)]) = WriterT(fa())
      protected def run[A](ga: WriterT[F, W, A])(n: Null) = ga.run
      protected def conv[A](fa: Null => F[(W, A)]) = () => fa(null)
    }

  implicit def readerTNonDeterminism[F[_]: Nondeterminism, E]: ND[ReaderT[F, E, ?]] =
    new NonDeterminismMonad[ReaderT[F, E, ?], F, Id.Id, Function1[E, ?], E] {
      protected def cons[A](fa: E => F[A]) = ReaderT(fa)
      protected def run[A](ga: ReaderT[F, E, A])(e: E) = ga.run(e)
      protected def conv[A](fa: E => F[A]) = fa
    }

  implicit def stateTNonDeterminism[F[_]: Nondeterminism, S]: ND[StateT[F, S, ?]] =
    new NonDeterminismMonad[StateT[F, S, ?], F, (S, ?), Function1[S, ?], S] {
      protected def cons[A](fa: S => F[(S, A)]) = StateT(fa)
      protected def run[A](ga: StateT[F, S, A])(s: S) = ga.run(s)
      protected def conv[A](fa: S => F[(S, A)]) = fa
    }

  implicit def ndNonDeterminism[F[_]](implicit nd: ND[F]): Nondeterminism[F] = new Nondeterminism[F] {
    override def chooseAny[A](head: F[A], tail: Seq[F[A]]): F[(A, Seq[F[A]])] = nd.chooseAny(head, tail)
    override def bind[A, B](fa: F[A])(f: (A) => F[B]): F[B] = nd.bind(fa)(f)
    override def point[A](a: => A): F[A] = nd.point(a)
  }
}
