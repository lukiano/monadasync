package io.atlassian.monadasync

import org.scalacheck.{ Gen, Arbitrary, Prop, Properties }
import org.specs2.scalaz.Spec

import scalaz._

trait MonadAsyncSpec extends org.specs2.mutable.SpecWithJUnit with Spec {

  type F[_]
  def MonadAsyncF: MonadAsync[F]
  def NondeterminismF: Nondeterminism[F]
  def run[A](f: F[A]): A

  implicit val pool = DefaultExecutor

  "MonadAsync" should {
    "not deadlock when using Nondeterminism#chooseAny" in {
      WithTimeout(2000) {
        run(deadlocks(3)).length must_== 4
      }
    }

    "have a run method that returns" >> {
      "when constructed from MonadAsync.now" in Prop.forAll { (n: Int) =>
        run(MonadAsyncF.now(n)) must_== n
      }
      "when constructed from MonadAsync.suspend" in Prop.forAll { (n: Int) =>
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

  def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(MonadAsyncF.now(i)),
        Gen.const(MonadAsyncF.delay(i)),
        Gen.const(MonadAsyncF.async(i))
      )
    }
  }

  lazy val arbitraryF1: Arbitrary[Int => F[Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const({ i: Int => MonadAsyncF.now(i) }),
      Gen.const({ i: Int => MonadAsyncF.delay(i) }),
      Gen.const({ i: Int => MonadAsyncF.async(i) })
    )
  }

  lazy val arbitraryInt: Arbitrary[Int] = Arbitrary.arbInt

  def arbitraryF0(implicit a: Arbitrary[Int]): Arbitrary[() => Int] = Arbitrary {
    a.arbitrary map { a => () => a }
  }

  def equalTc: Equal[F[Int]] =
    new Equal[F[Int]] {
      import scalaz.std.anyVal.intInstance
      override def equal(tc1: F[Int], tc2: F[Int]): Boolean = intInstance.equal(run(tc1), run(tc2))
    }

  def laws: Properties
}