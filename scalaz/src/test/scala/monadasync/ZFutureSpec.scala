package monadasync
import scalaz._
import scalaz.concurrent.Future
import scalaz.scalacheck.ScalazProperties
import MonadAsync.FutureMonadAsync
import Future._

object Helper {
  implicit object CatchableFuture extends Catchable[Future] {
    override def attempt[A](f: Future[A]): Future[Throwable \/ A] =
      f map \/-.apply
    override def fail[A](err: Throwable): Future[A] =
      ???
  }
}

import Helper._

object ZFutureSpec extends MonadAsyncSpec[Future] {

  override def run[A](f: Future[A]): A =
    f.run

  override val laws = MonadAsyncProperties.monadAsync.laws[Future]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[Future])
}
