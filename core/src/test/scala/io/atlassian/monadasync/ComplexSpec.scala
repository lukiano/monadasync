package io.atlassian.monadasync

import scalaz._
import scalaz.scalacheck.ScalazProperties
import scalaz.std.anyVal._
import scalaz.concurrent.Future
import Future._
import MonadAsync._
import Nondeterminisms._

object ComplexSpec extends MonadAsyncSpec {
  type Task[A] = EitherT[Future, Throwable, A]
  type WrittenTask[A] = WriterT[Task, Int, A]
  override type F[A] = ReaderT[WrittenTask, Unit, A]

  override def run[A](f: F[A]): A =
    f.run(()).value.run.run.toOption.get

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = {
    import scalaz.syntax.monoid._
    import scalaz.syntax.functor._
    implicit object CatchableTask extends Catchable[Task] {
      override def attempt[A](f: Task[A]): Task[Throwable \/ A] =
        EitherT[Future, Throwable, Throwable \/ A](f.run map \/-.apply)
      override def fail[A](err: Throwable): Task[A] =
        EitherT[Future, Throwable, A](Future.now(-\/(err)))
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

    Kleisli.kleisliCatchable[WrittenTask, Unit]
  }

  override val laws = MonadAsyncProperties.monadAsync.laws[F](
    MonadAsyncF,
    arbitraryTC,
    arbitraryInt,
    arbitraryF0,
    arbitraryF1,
    equalTc
  )

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])
}
