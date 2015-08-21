package io.atlassian.monadasync

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Prop._
import org.scalacheck.{ Gen, Arbitrary, Prop }
import org.typelevel.discipline.Laws
import org.typelevel.discipline.specs2.mutable.Discipline

object MonadAsyncSpec extends org.specs2.mutable.SpecificationWithJUnit with Discipline with Async {

  import MonadAsyncF._
  import cats.std.int._

  implicit val pool = DefaultExecutor

  "MonadAsync" should {
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

  implicit def EqualTC[A](implicit e: Eq[A]): Eq[StateTask[A]] =
    new Eq[StateTask[A]] {
      override def eqv(tc1: MonadAsyncSpec.StateTask[A], tc2: MonadAsyncSpec.StateTask[A]): Boolean = e.eqv(run(tc1), run(tc2))
    }

  implicit val ArbitraryTC: ArbitraryK[StateTask] = new ArbitraryK[StateTask] {
    def synthesize[A: Arbitrary]: Arbitrary[StateTask[A]] = Arbitrary {
      Arbitrary.arbitrary[A] flatMap { a =>
        Gen.oneOf(MonadAsyncF.now(a), MonadAsyncF.delay(a))
      }
    }
  }

  checkAll("AwsActionMonad Monad laws", MonadAsyncTests[StateTask](MonadAsyncF).monadAsync[Int, Int, Int])
}

trait MonadAsyncTests[F[_]] extends Laws {

  private implicit def arbFunction0[R](implicit a: Arbitrary[R]): Arbitrary[() => R] =
    ArbitraryK.function0.synthesize[R]

  def laws: MonadAsyncLaws[F]

  def monadAsync[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit ArbF: ArbitraryK[F],
                                                           EqFA: Eq[F[A]],
                                                           EqFB: Eq[F[B]],
                                                           EqFC: Eq[F[C]]): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]
    implicit def ArbFC: Arbitrary[F[C]] = ArbF.synthesize[C]
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]

    new DefaultRuleSet(
      name = "monadAsync",
      parent = None,
      "asyncIsDelay" -> forAll(laws.asyncIsDelay[A] _),
      "bindAIsBind" -> forAll(laws.bindAIsBind[A, B] _),
      "mapAIsMap" -> forAll(laws.mapAIsMap[A, B] _)
    )
  }
}

object MonadAsyncTests {
  def apply[F[_]](implicit MA: MonadAsync[F]): MonadAsyncTests[F] =
    new MonadAsyncTests[F] { def laws: MonadAsyncLaws[F] = MonadAsyncLaws[F] }
}
