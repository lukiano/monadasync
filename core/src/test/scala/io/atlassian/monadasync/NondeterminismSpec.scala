package io.atlassian.monadasync

import java.util.concurrent.Executors

import scalaz.concurrent.Future
import scalaz.std.anyVal._
import scalaz._
import scalaz.std.set._
import Future._
import MonadAsync._
import Nondeterminisms._

object NondeterminismSpec extends org.specs2.mutable.SpecificationWithJUnit {

  implicit val es = Executors.newSingleThreadScheduledExecutor

  type Task[A] = EitherT[Future, Throwable, A]
  type WrittenTask[A] = WriterT[Task, Int, A]
  type F[A] = ReaderT[WrittenTask, Unit, A]

  def run[A](f: F[A]): A =
    f.run(()).value.run.run.toOption.get

  val MonadAsyncF = MonadAsync[F]

  val NondeterminismF = Nondeterminism[F]

  "Nondeterminism[F]" should {
    import MonadAsyncF._
    import NondeterminismF._
    val intSetReducer = Reducer.unitReducer[Int, Set[Int]](Set(_))

    "correctly process reduceUnordered for >1 futures in non-blocking way" in {
      val f1 = fork(now(1))
      val f2 = bind(delay(7)) { _ => fork(now(2)) }
      val f3 = fork(now(3))

      val f = fork(reduceUnordered(Seq(f1, f2, f3))(intSetReducer))

      run(f) must_== Set(1, 2, 3)
    }

    "correctly process reduceUnordered for 1 future in non-blocking way" in {
      val f1 = fork(now(1))

      val f = fork(reduceUnordered(Seq(f1))(intSetReducer))

      run(f) must_== Set(1)
    }

    "correctly process reduceUnordered for empty seq of futures in non-blocking way" in {
      val f = fork(reduceUnordered(Seq())(intSetReducer))

      run(f) must_== Set()
    }
  }

}
