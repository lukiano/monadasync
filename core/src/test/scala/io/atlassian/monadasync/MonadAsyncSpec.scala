package io.atlassian.monadasync

import org.scalacheck.{ Gen, Arbitrary, Prop, Properties }
import org.specs2.scalaz.Spec

import scalaz._

trait MonadAsyncSpec extends org.specs2.mutable.SpecWithJUnit with Spec {

  type F[_]
  def MonadAsyncF: MonadAsync[F]
  def NondeterminismF: Nondeterminism[F]
  def CatchableF: Catchable[F]
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
      val m = MonadAsyncF
      import m.monad
      implicit val c = CatchableF
      val maxValue = 100000
      run(Process.range(1, maxValue).asInstanceOf[Process[F, Int]].runFoldMap[F, Int](identity)) must_== (1 until maxValue).toIndexedSeq.suml
    }

    "have some run method that returns" >> {
      "when constructed from MonadAsync.now" in Prop.forAll { (n: Int) =>
        run(MonadAsyncF.now(n)) must_== n
      }
      "when constructed from MonadAsync.delay" in Prop.forAll { (n: Int) =>
        run(MonadAsyncF.delay(n)) must_== n
      }
      "when constructed from MonadAsync.fork" in Prop.forAll { (n: Int) =>
        run(MonadAsyncF.fork(MonadAsyncF.now(n))) must_== n
      }
      "when constructed from MonadAsync.async" in Prop.forAll { (n: Int) =>
        def callback(call: Int => Unit): Unit = call(n)
        run(MonadAsyncF.async(callback _)) must_== n
      }
      "when constructed from MonadAsync.apply" in Prop.forAll { (n: Int) =>
        run(MonadAsyncF.async(n)) must_== n
      }
    }
  }

  "Timed MonadAsync" should {
    "not run sequentially" in {
      val times = Stream.iterate(100l)(_ + 100).take(10)

      val start = System.currentTimeMillis()
      val result = run(MonadAsyncF.fork(NondeterminismF.gatherUnordered(times.map { time =>
        MonadAsyncF.fork {
          Thread.sleep(time)
          MonadAsyncF.now(time)
        }
      })))
      val duration = System.currentTimeMillis() - start
      (result.length must_== times.size) and (duration must be_<(times.sum))
    }
  }

  def deadlocks(depth: Int): F[List[Long]] =
    if (depth == 1) {
      MonadAsyncF.fork(
        MonadAsyncF.delay {
          Thread.sleep(20)
          List(System.currentTimeMillis)
        }
      )
    } else {
      MonadAsyncF.fork(
        NondeterminismF.map(NondeterminismF.both(deadlocks(depth - 1), deadlocks(depth - 1))) {
          case (l, r) => l ++ r
        }
      )
    }

  implicit def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(MonadAsyncF.now(i)),
        Gen.const(MonadAsyncF.delay(i)),
        Gen.const(MonadAsyncF.async(i))
      )
    }
  }

  implicit val arbitraryF1: Arbitrary[Int => F[Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const({ i: Int => MonadAsyncF.now(i) }),
      Gen.const({ i: Int => MonadAsyncF.delay(i) }),
      Gen.const({ i: Int => MonadAsyncF.async(i) })
    )
  }

  implicit val arbitraryF2: Arbitrary[F[Int => Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const(MonadAsyncF.now({ i: Int => i })),
      Gen.const(MonadAsyncF.delay({ i: Int => i })),
      Gen.const(MonadAsyncF.async({ i: Int => i }))
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
        Equal[Throwable \/ Int].equal(run(CatchableF.attempt(tc1)), run(CatchableF.attempt(tc2)))
    }

  def laws: Properties
}