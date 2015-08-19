package io.atlassian.monadasync

import cats.Eq
import cats.laws.discipline.{ EqK, ArbitraryK, ComonadTests, MonadTests }
import org.scalacheck.{ Gen, Arbitrary }
import org.specs2.mutable.SpecificationWithJUnit
import org.typelevel.discipline.specs2.mutable.Discipline

object FutureSpec extends SpecificationWithJUnit with Discipline {
  import cats.std.int._

  implicit val EqualFutureK: EqK[Future] =
    new EqK[Future] {
      override def synthesize[A](implicit eqa: Eq[A]): Eq[Future[A]] = EqualFuture
    }

  implicit def EqualFuture[A](implicit e: Eq[A]): Eq[Future[A]] =
    new Eq[Future[A]] {
      override def eqv(f1: Future[A], f2: Future[A]): Boolean = e.eqv(f1.run, f2.run)
    }
  implicit val ArbitraryFuture: ArbitraryK[Future] = new ArbitraryK[Future] {
    def synthesize[A: Arbitrary]: Arbitrary[Future[A]] = Arbitrary {
      Arbitrary.arbitrary[A] flatMap { a =>
        Gen.oneOf(Future.now(a), Future.delay(a))
      }
    }
  }

  checkAll("Future Monad laws", MonadTests[Future].monad[Int, Int, Int])
  checkAll("Future Comonad laws", ComonadTests[Future].comonad[Int, Int, Int])
}
