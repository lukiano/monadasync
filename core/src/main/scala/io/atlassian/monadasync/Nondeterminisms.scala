package io.atlassian.monadasync

import scala.util.Try
import scalaz._
import scalaz.syntax.monad._

object Nondeterminisms {
  implicit def EitherTNonDeterminism[F[_]: Nondeterminism, L] = new Nondeterminism[EitherT[F, L, ?]] {
    override def point[A](a: => A) =
      a.point[EitherT[F, L, ?]]
    override def bind[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]) =
      fa >>= f
    override def chooseAny[A](head: EitherT[F, L, A], tail: Seq[EitherT[F, L, A]]) =
      EitherT {
        Nondeterminism[F].chooseAny(head.run, tail.map(_.run)) map {
          case (disj, seq) => disj.map { a => (a, seq.map(EitherT.apply)) }
        }
      }
  }

  implicit def WriterTNondeterminism[F[_]: Nondeterminism, W: Monoid] = new Nondeterminism[WriterT[F, W, ?]] {
    override def point[A](a: => A) =
      a.point[WriterT[F, W, ?]]
    override def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) =
      fa >>= f
    override def chooseAny[A](head: WriterT[F, W, A], tail: Seq[WriterT[F, W, A]]) =
      WriterT(Nondeterminism[F].chooseAny(head.run, tail.map(_.run)) map {
        case ((w, a), seq) => (w, (a, seq map WriterT.writerT))
      })
  }

  implicit def ReaderTNondeterminism[F[_]: Nondeterminism, E] = new Nondeterminism[ReaderT[F, E, ?]] {
    override def point[A](a: => A) =
      a.point[ReaderT[F, E, ?]]
    override def bind[A, B](fa: ReaderT[F, E, A])(f: A => ReaderT[F, E, B]) =
      fa >>= f
    override def chooseAny[A](head: ReaderT[F, E, A], tail: Seq[ReaderT[F, E, A]]) =
      Kleisli { e =>
        Nondeterminism[F].chooseAny(head.run(e), tail.map(_.run(e))) map {
          case (a, seq) => (a, seq map { elem => Kleisli.kleisli[F, E, A](_ => elem) })
        }
      }
  }

  implicit def StateTNondeterminism[F[_]: Nondeterminism, S] = new Nondeterminism[StateT[F, S, ?]] {
    override def point[A](a: => A) =
      a.point[StateT[F, S, ?]]
    override def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]) =
      fa >>= f
    override def chooseAny[A](head: StateT[F, S, A], tail: Seq[StateT[F, S, A]]) =
      StateT { s =>
        Nondeterminism[F].chooseAny(head.run(s), tail.map(_.run(s))) map {
          case ((ns, a), seq) => (ns, (a, seq map { elem => StateT[F, S, A](_ => elem) }))
        }
      }
  }
}
