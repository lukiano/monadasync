package io.atlassian.monadasync

import org.scalacheck.Prop
import org.specs2.ScalaCheck

import scalaz._
import scalaz.concurrent.Future
import scalaz.std.anyVal._
import WriterT._
import EitherT._

object RetrySpec extends org.specs2.mutable.SpecificationWithJUnit with ScalaCheck {

   implicit val pool = DefaultExecutor

   type EitherFixed[A] = EitherT[Future, Throwable, A]
   type TC[A] = WriterT[EitherFixed, Int, A]

   def run[A](f: TC[A]): Option[A] =
     f.value.run.run.toOption

   implicit val C = new Catchable[TC] {
     override def attempt[A](f: TC[A]): TC[Throwable \/ A] =
       WriterT[EitherFixed, Int, Throwable \/ A](EitherT(f.run.run.map {
         case -\/(t) => \/-((0, -\/(t)))
         case \/-((w, a)) => \/-((w, \/-(a)))
       }))
     override def fail[A](err: Throwable): TC[A] =
       WriterT[EitherFixed, Int, A](EitherT[Future, Throwable, (Int,A)](Future.now(-\/(err))))
   }

   "retries a retriable task n times" ! Prop.forAll { xs: List[Byte] =>
     import scala.concurrent.duration._
     import scalaz.syntax.monad._
     import Retry.catchable._

     var x = 0
     val errorMessage = "can be repeated"
     val f: TC[Unit] = MonadAsync[TC].delay { x += 1 } >> C.fail(new Exception(errorMessage))

     val withRetries: TC[Unit] = f.retry(xs.map(_ => 0.milliseconds), { _.getMessage == errorMessage })
     run(withRetries)

     x === (xs.length + 1)
   }.set(minTestsOk = 8, workers = Runtime.getRuntime.availableProcessors)
 }
