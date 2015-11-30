package io.atlassian.monadasync
package twitter

import com.twitter.util.Future

import scalaz.scalacheck.ScalazProperties
import scalaz.{ Catchable, Comonad, Nondeterminism }

object FutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

  override def run[A](f: F[A]): A =
    Comonad[F].copoint(f)

  override val MonadAsyncF = MonadAsync[F]
  override val NondeterminismF = Nondeterminism[F]
  override val CatchableF = Catchable[F]

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])

  checkAll("MonadPlus laws", ScalazProperties.monadPlus.strongLaws[F])

  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => F[A]], Throwable])

  checkAll("Comonad laws", ScalazProperties.comonad.laws[F])
}
