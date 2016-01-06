package monadasync
import java.util.concurrent.CompletableFuture

import JavaFuture._

import scalaz._
import scalaz.scalacheck.ScalazProperties

object JavaFutureSpec extends MonadAsyncSpec[CompletableFuture] {

  override def run[A](f: CompletableFuture[A]): A =
    Comonad[CompletableFuture].copoint(f)

  override val laws = MonadAsyncProperties.monadAsync.laws[CompletableFuture]

  checkAll("MonadAsync laws", laws)

  checkAll("Comonad laws", ScalazProperties.comonad.laws[CompletableFuture])

  checkAll("Zip laws", ScalazProperties.zip.laws[CompletableFuture])

  //  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => CompletableFuture[A]], Throwable])
}
