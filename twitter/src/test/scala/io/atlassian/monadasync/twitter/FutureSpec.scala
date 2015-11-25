package io.atlassian.monadasync
package twitter

import com.twitter.util.Future
import org.scalacheck.{ Gen, Arbitrary }

import scalaz.scalacheck.ScalazProperties
import scalaz.{ Comonad, Nondeterminism }

class FutureSpec extends MonadAsyncSpec {

  override type F[A] = Future[A]

  override def run[A](f: F[A]): A =
    Comonad[F].copoint(f)

  override val MonadAsyncF = MonadAsync[F]

  override val NondeterminismF = Nondeterminism[F]

  override val laws = MonadAsyncProperties.monadAsync.laws[F]

  implicit val arbitraryF2: Arbitrary[F[Int => Int]] = Arbitrary {
    Gen.oneOf(
      Gen.const(MonadAsyncF.now({ i: Int => i })),
      Gen.const(MonadAsyncF.delay({ i: Int => i })),
      Gen.const(MonadAsyncF.async({ i: Int => i }))
    )
  }

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[F])

  checkAll("MonadPlus laws", ScalazProperties.monadPlus.laws[F])

  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => F[A]], Throwable])

  checkAll("Comonad laws", ScalazProperties.comonad.laws[F])
}
