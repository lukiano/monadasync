package monadasync
import org.scalacheck.{ Gen, Arbitrary, Prop, Properties }
import org.specs2.scalaz.Spec

import scalaz._
import scalaz.syntax.catchable._
import scalaz.syntax.functor._
import MonadAsync.syntax._

abstract class MonadAsyncSpec[F[_]: MonadAsync: Nondeterminism: Catchable] extends org.specs2.mutable.SpecWithJUnit with Spec {

  def run[A](f: F[A]): A

  implicit val pool = DefaultExecutor

  "MonadAsync" should {
    "not deadlock when using Nondeterminism#chooseAny" in {
      WithTimeout(2000) {
        run(deadlocks(3)).length must_== 4
      }
    }
    "not overflow when using with scalaz-stream Process" in {
      import scalaz.stream.Process
      import scalaz.std.anyVal.intInstance
      import scalaz.std.indexedSeq.indexedSeqInstance
      import scalaz.syntax.foldable._
      val maxValue = 100000
      run(Process.range(1, maxValue).asInstanceOf[Process[F, Int]].runFoldMap[F, Int](identity)) must_== (1 until maxValue).toIndexedSeq.suml
    }

    "have some run method that returns" >> {
      "when constructed from MonadAsync.now" in Prop.forAll { (n: Int) =>
        run(n.now[F]) must_== n
      }
      "when constructed from MonadAsync.delay" in Prop.forAll { (n: Int) =>
        run(delay[F, Int](n)) must_== n
      }
      "when constructed from MonadAsync.fork" in Prop.forAll { (n: Int) =>
        run(fork(n.now[F])) must_== n
      }
      "when constructed from MonadAsync.async" in Prop.forAll { (n: Int) =>
        def cb(call: Int => Unit): Unit = call(n)
        run(callback[F, Int](cb)) must_== n
      }
      "when constructed from MonadAsync.apply" in Prop.forAll { (n: Int) =>
        run(async[F, Int](n)) must_== n
      }
    }
  }

  "Timed MonadAsync" should {
    "not run sequentially" in {
      val times = Stream.iterate(100l)(_ + 100).take(10)

      val start = System.currentTimeMillis()
      val result = run(fork(Nondeterminism[F].gatherUnordered(times map { time =>
        fork {
          Thread.sleep(time)
          time.now[F]
        }
      })))
      val duration = System.currentTimeMillis() - start
      (result.length must_== times.size) and (duration must be_<(times.sum))
    }
  }

  def deadlocks(depth: Int): F[List[Long]] =
    if (depth == 1) {
      fork(
        delay[F, List[Long]] {
          Thread.sleep(20)
          List(System.currentTimeMillis)
        }
      )
    } else {
      fork(
        Nondeterminism[F].both(deadlocks(depth - 1), deadlocks(depth - 1)) map {
          case (l, r) => l ++ r
        }
      )
    }

  implicit def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(i.now[F]),
        Gen.const(delay[F, Int](i)),
        Gen.const(async[F, Int](i))
      )
    }
  }

  implicit val arbitraryF1: Arbitrary[Int => F[Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const({ i: Int => i.now[F] }),
      Gen.const({ i: Int => delay[F, Int](i) }),
      Gen.const({ i: Int => async[F, Int](i) })
    )
  }

  implicit val arbitraryF2: Arbitrary[F[Int => Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const({ i: Int => i }.now[F]),
      Gen.const(delay[F, Int => Int]({ i: Int => i })),
      Gen.const(async[F, Int => Int]({ i: Int => i }))
    )
  }

  implicit val arbitraryInt: Arbitrary[Int] = Arbitrary.arbInt

  implicit def arbitraryF0(implicit a: Arbitrary[Int]): Arbitrary[() => Int] = Arbitrary {
    a.arbitrary map { a => () => a }
  }

  implicit val equalTc: Equal[F[Int]] =
    new Equal[F[Int]] {
      import scalaz.std.anyVal.intInstance
      import scalaz.\/.DisjunctionEqual
      private implicit val throwableEqual = Equal.equalA[Throwable]
      override def equal(tc1: F[Int], tc2: F[Int]): Boolean =
        Equal[Throwable \/ Int].equal(run(Catchable[F].attempt(tc1)), run(tc2.attempt))
    }

  def laws: Properties
}