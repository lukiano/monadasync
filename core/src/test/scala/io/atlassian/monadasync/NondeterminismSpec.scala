package io.atlassian.monadasync

import java.util.concurrent.Executors

import scalaz.Reducer
import scalaz.std.set._

object NondeterminismSpec extends org.specs2.mutable.SpecificationWithJUnit with Async {

  implicit val es = Executors.newSingleThreadScheduledExecutor

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
