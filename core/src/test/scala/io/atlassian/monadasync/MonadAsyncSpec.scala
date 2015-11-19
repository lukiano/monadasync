package io.atlassian.monadasync

import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Prop, Properties }
import org.specs2.scalaz.Spec

import scalaz._

object MonadAsyncSpec extends org.specs2.mutable.SpecWithJUnit with Spec with Async {

  import MonadAsyncF._
  import NondeterminismF._

  implicit val pool = DefaultExecutor

  "MonadAsync" should {
    "not deadlock when using Nondeterminism#chooseAny" in {
      WithTimeout(2000) {
        run(deadlocks(3)).length must_== 4
      }
    }

    "have a run method that returns" >> {
      "when constructed from MonadAsync.now" in Prop.forAll { (n: Int) =>
        run(now(n)) must_== n
      }
      "when constructed from MonadAsync.suspend" in Prop.forAll { (n: Int) =>
        run(delay(n)) must_== n
      }
      "when constructed from MonadAsync.fork" in Prop.forAll { (n: Int) =>
        run(fork(now(n))) must_== n
      }
      "when constructed from MonadAsync.async" in Prop.forAll { (n: Int) =>
        def callback(call: Int => Unit): Unit = call(n)
        run(async(callback _)) must_== n
      }
      "when constructed from MonadAsync.apply" in Prop.forAll { (n: Int) =>
        run(async(n)) must_== n
      }
    }
  }

  "Timed MonadAsync" should {
    "not run sequentially" in {
      val times = Stream.iterate(100l)(_ + 100).take(10)

      val start = System.currentTimeMillis()
      val result = run(fork(gatherUnordered(times.map { time =>
        fork {
          Thread.sleep(time)
          now(time)
        }
      })))
      val duration = System.currentTimeMillis() - start
      (result.length must_== times.size) and (duration must be_<(times.sum))
    }
  }

  def deadlocks(depth: Int): Example[List[Long]] =
    if (depth == 1)
      fork(
        delay {
          Thread.sleep(20)
          List(System.currentTimeMillis)
        }
      )
    else
      fork(
        NondeterminismF.map(both(deadlocks(depth - 1), deadlocks(depth - 1))) {
          case (l, r) => l ++ r
        }
      )

  implicit val MonadAsyncTF = MonadAsyncF

  implicit def ArbitraryTC[A](implicit a: Arbitrary[A]): Arbitrary[Example[A]] = Arbitrary {
    a.arbitrary map MonadAsyncF.now
  }

  implicit def ArbitraryF0[A](implicit a: Arbitrary[A]): Arbitrary[() => A] = Arbitrary {
    a.arbitrary map { a => () => a }
  }

  import scalaz.std.anyVal._

  implicit def EqualTc[A](implicit e: Equal[A]): Equal[Example[A]] =
    new Equal[Example[A]] {
      override def equal(tc1: MonadAsyncSpec.Example[A], tc2: MonadAsyncSpec.Example[A]): Boolean = e.equal(run(tc1), run(tc2))
    }

  checkAll("AwsActionMonad Monad laws", MonadAsyncProperties.monadAsync.laws[Example])

}

object MonadAsyncProperties {

  object monadAsync {
    def asyncIsDelay[F[_], A](implicit ma: MonadAsync[F], eq: Equal[F[A]], a0: Arbitrary[() => A]) =
      forAll(ma.monadAsyncLaw.asyncIsDelay[A] _)
    def bindAIsBind[F[_], A, B](implicit ma: MonadAsync[F], eq: Equal[F[B]], a: Arbitrary[A], afa: Arbitrary[A => F[A]], afb: Arbitrary[A => F[B]]) =
      forAll(ma.monadAsyncLaw.bindAIsBind[A, B] _)
    def mapAIsMap[F[_], A, B](implicit ma: MonadAsync[F], eq: Equal[F[B]], a: Arbitrary[A], afa: Arbitrary[A => F[A]], ab: Arbitrary[A => B]) =
      forAll(ma.monadAsyncLaw.mapAIsMap[A, B] _)

    def laws[M[_]](implicit ma: MonadAsync[M], am: Arbitrary[M[Int]], a: Arbitrary[Int], am0: Arbitrary[() => Int],
      ama: Arbitrary[Int => M[Int]], e: Equal[M[Int]]) =
      new Properties("monad async") {
        property("asyncIsDelay") = asyncIsDelay[M, Int]
        property("bindAIsBind") = bindAIsBind[M, Int, Int]
        property("mapAIsMap") = mapAIsMap[M, Int, Int]
      }

  }

}
