package io.atlassian.monadasync
package twitter

import com.twitter.util.Future
import org.scalacheck.{ Gen, Prop, Properties }

import scalaz.std.anyVal.intInstance
import scalaz.scalacheck.ScalazProperties
import scalaz.{ std, Catchable, Comonad, Nondeterminism }

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

  checkAll("Zip laws", ScalazProperties.zip.laws[F])

  checkAll("Traverse laws", traverseLaws)

  def traverseLaws: Properties = new Properties("traverse") {
    private def resizeProp(p: Prop, max: Int): Prop = new Prop {
      def apply(params: Gen.Parameters) =
        p(params.withSize(params.size % (max + 1)))
    }

    include(ScalazProperties.functor.laws[F])
    include(ScalazProperties.foldable.laws[F])
    property("identity traverse") = ScalazProperties.traverse.identityTraverse[F, Int, Int]

    import std.list._, std.option._, std.stream._

    property("purity.option") = ScalazProperties.traverse.purity[F, Option, Int]
    property("purity.stream") = ScalazProperties.traverse.purity[F, Stream, Int]

    property("sequential fusion") =
      resizeProp(ScalazProperties.traverse.sequentialFusion[F, Option, List, Int, Int, Int], 3)
  }
}
