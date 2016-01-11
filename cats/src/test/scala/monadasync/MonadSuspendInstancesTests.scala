package monadasync

import org.scalacheck.{ Gen, Arbitrary }
import cats.{ Monad, Eq, Eval }
import org.scalacheck.Prop.forAll

import cats.free.Trampoline

abstract class MonadSuspendInstancesTests[F[_]](laws: MonadSuspendLaws[F]) extends Laws {
  def monadSuspend[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], ArbF0: Arbitrary[() => A], EqFA: Eq[F[A]]): RuleSet = {
    new SimpleRuleSet(
      name = "monadsuspend",
      "now is point" -> forAll(laws.nowIsPoint[A] _),
      "delay is map" -> forAll(laws.delayIsMap[A] _),
      "suspend is delay + join" -> forAll(laws.suspendIsDelayJoin[A] _)
    )
  }
}

object MonadSuspendInstancesTests extends Spec {
  def apply[F[_]: MonadSuspend: Monad]: MonadSuspendInstancesTests[F] =
    new MonadSuspendInstancesTests[F](MonadSuspendLaws.monadSuspendLaw[F]) {}
}

abstract class MonadSuspendSpec[F[_]: MonadSuspend: Monad] extends Spec {
  def run[A](f: F[A]): A

  def name: String

  implicit def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    import MonadSuspend.syntax._
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(i.now[F]),
        Gen.const(delay[F, Int](i))
      )
    }
  }

  implicit def eqTC[A]: Eq[F[A]] = Eq.instance {
    case (f1, f2) => run(f1) == run(f2)
  }

  checkAll(name, MonadSuspendInstancesTests[F].monadSuspend[Int])
}

class TrampolineSpec extends MonadSuspendSpec[Trampoline] {
  import cats.std.function.function0Instance
  def name = "Trampoline"
  def run[A](f: Trampoline[A]) = f.run
}

class EvalSpec extends MonadSuspendSpec[Eval] {
  def name = "Eval"
  def run[A](f: Eval[A]) = f.value
}
