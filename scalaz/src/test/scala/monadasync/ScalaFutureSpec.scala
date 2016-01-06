package monadasync
import scalaz._
import scala.concurrent.Future
import ScalaFuture._

import scalaz.scalacheck.ScalazProperties

object ScalaFutureSpec extends MonadAsyncSpec[Future] {

  override def run[A](f: Future[A]): A =
    Comonad[Future].copoint(f)

  override val laws = MonadAsyncProperties.monadAsync.laws[Future]

  checkAll("MonadAsync laws", laws)

  checkAll("Comonad laws", ScalazProperties.comonad.laws[Future])

  checkAll("Zip laws", ScalazProperties.zip.laws[Future])

  //  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => Future[A]], Throwable])

  //  checkAll("MonadPlus laws", ScalazProperties.monadPlus.strongLaws[Future])
}
