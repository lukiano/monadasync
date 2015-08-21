package io.atlassian.monadasync

import cats.data.XorT
import cats.state.StateT
import org.scalacheck.Prop
import org.specs2.ScalaCheck

import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._
import Future._
import Retry.monadError._

object RetrySpec extends org.specs2.mutable.SpecificationWithJUnit with ScalaCheck {

  type Failure = String
  type EitherFixed[A] = XorT[Future, Failure, A]
  type TC[A] = StateT[EitherFixed, Int, A]

  def run[A](f: TC[A]): Option[A] =
    f.runA(0).value.map(_.toOption).run

  implicit val ME = new MonadError[({ type l[α, β] = TC[β] })#l, Failure] {

    private val e = MonadError[XorT[Future, ?, ?], Failure]
    private val w = StateT.stateTMonadState[EitherFixed, Int]
    override def raiseError[A](f: Failure): TC[A] =
      StateT[EitherFixed, Int, A](_ => e.raiseError(f))
    override def handleError[A](fa: TC[A])(f: Failure => TC[A]): TC[A] =
      StateT[EitherFixed, Int, A](s => e.handleError(fa.run(s)) { i => f(i).run(s) })
    override def pure[A](a: A): TC[A] =
      w.pure(a)
    override def flatMap[A, B](fa: TC[A])(f: A => TC[B]): TC[B] =
      w.flatMap(fa)(f)
  }

  "retries a retriable task n times" ! Prop.forAll { xs: List[Byte] =>
    import scala.concurrent.duration._

    var x = 0
    val errorMessage = "can be repeated"
    val f: TC[Unit] = MonadAsync[TC].delay { x += 1 } >> ME.raiseError(errorMessage)

    val withRetries: TC[Unit] = f.retry(xs.map(_ => 0.milliseconds), { _ == errorMessage })
    run(withRetries)

    x === (xs.length + 1)
  }.set(minTestsOk = 8, workers = Runtime.getRuntime.availableProcessors)
}
