package monadasync
import org.scalacheck.{ Gen, Arbitrary }
import org.specs2.scalaz.Spec
import scalaz.{ Equal, Free }
import Free.Trampoline

object TrampolineSpec extends org.specs2.mutable.SpecWithJUnit with Spec {

  type F[A] = Trampoline[A]
  implicit val MonadSuspendF: MonadSuspend[F] = MonadSuspend.TrampolineMonadSuspend
  def run[A](f: F[A]): A = f.run

  implicit val equalTc: Equal[F[Int]] =
    new Equal[F[Int]] {
      import scalaz.std.anyVal.intInstance
      override def equal(tc1: F[Int], tc2: F[Int]): Boolean =
        Equal[Int].equal(run(tc1), run(tc2))
    }

  implicit def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(MonadSuspendF.now(i)),
        Gen.const(MonadSuspendF.delay(i))
      )
    }
  }

  implicit def arbitraryF0(implicit a: Arbitrary[Int]): Arbitrary[() => Int] = Arbitrary {
    a.arbitrary map { a => () => a }
  }

  checkAll("MonadSuspend laws", MonadAsyncProperties.monadSuspend.laws[F])
}
