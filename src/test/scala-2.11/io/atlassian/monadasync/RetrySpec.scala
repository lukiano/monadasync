package io.atlassian.monadasync

import org.scalacheck.Prop
import org.specs2.ScalaCheck

import scalaz._
import scalaz.concurrent.Future
import scalaz.std.anyVal._

object RetrySpec extends org.specs2.mutable.SpecificationWithJUnit with ScalaCheck {

  implicit val pool = DefaultExecutor

  type Failure = String
  type EitherFixed[A] = EitherT[Future, Failure, A]
  type TC[A] = WriterT[EitherFixed, Int, A]

  def run[A](f: TC[A]): Option[A] =
    f.value.run.run.toOption

  implicit val ME = new MonadError[({ type l[α, β] = TC[β] })#l, Failure] {

    private val e = MonadError[EitherT[Future, ?, ?], Failure]
    private val w = WriterT.writerTMonad[EitherFixed, Int]
    override def raiseError[A](f: Failure): TC[A] =
      WriterT[EitherFixed, Int, A](e.raiseError(f))
    override def handleError[A](fa: TC[A])(f: Failure => TC[A]): TC[A] =
      WriterT[EitherFixed, Int, A](e.handleError(fa.run) { i => f(i).run })
    override def point[A](a: => A): TC[A] =
      w.point(a)
    override def bind[A, B](fa: TC[A])(f: A => TC[B]): TC[B] =
      w.bind(fa)(f)
  }

  "retries a retriable task n times" ! Prop.forAll { xs: List[Byte] =>
    import scala.concurrent.duration._
    import scalaz.syntax.monad._
    import Retry.monadError._

    var x = 0
    val errorMessage = "can be repeated"
    val f: TC[Unit] = MonadAsync[TC].suspend { x += 1 } >>= { _ => ME.raiseError(errorMessage) }

    val withRetries: TC[Unit] = f.retry(xs.map(_ => 0.milliseconds), { _ == errorMessage })
    run(withRetries)

    x === (xs.length + 1)
  }.set(minTestsOk = 8, workers = Runtime.getRuntime.availableProcessors)
}
