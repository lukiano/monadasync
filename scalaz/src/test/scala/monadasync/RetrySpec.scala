package monadasync
import org.scalacheck.Prop
import org.specs2.ScalaCheck

import scalaz._
import scalaz.concurrent.Future
import scalaz.std.anyVal._

object RetrySpec extends org.specs2.mutable.SpecificationWithJUnit with ScalaCheck {
  import Catchables._
  import MonadAsync._

  type EitherFixed[A] = EitherT[Future, Throwable, A]
  type TC[A] = WriterT[EitherFixed, Int, A]

  def run[A](f: TC[A]): Option[A] =
    f.value.run.run.toOption

  "retries a retriable task n times" ! Prop.forAll { xs: List[Byte] =>
    import scala.concurrent.duration._
    import scalaz.syntax.monad._
    import Retry.catchable._

    var x = 0
    val errorMessage = new Exception("can be repeated")
    val f: TC[Unit] = MonadAsync[TC].delay { x += 1 } >> Catchable[TC].fail(errorMessage)

    val withRetries: TC[Unit] = f.retry(xs.map(_ => 0.milliseconds), { _ == errorMessage })
    run(withRetries)

    x === (xs.length + 1)
  }.set(minTestsOk = 8, workers = Runtime.getRuntime.availableProcessors)
}
