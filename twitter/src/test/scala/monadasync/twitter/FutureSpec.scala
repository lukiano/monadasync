package monadasync
package twitter

import com.twitter.util.Future
import org.scalacheck.{ Gen, Prop, Properties }

import scalaz.scalacheck.ScalazProperties
import scalaz.{ std, Comonad }
import std.anyVal.intInstance
import scalaz._

object FutureSpec extends MonadAsyncSpec[Future] {

  override def run[A](f: Future[A]): A =
    Comonad[Future].copoint(f)

  override val laws = MonadAsyncProperties.monadAsync.laws[Future]

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[Future])

  checkAll("MonadPlus laws", ScalazProperties.monadPlus.strongLaws[Future])

  checkAll("MonadError laws", ScalazProperties.monadError.laws[Lambda[(?, A) => Future[A]], Throwable])

  checkAll("Comonad laws", ScalazProperties.comonad.laws[Future])

  checkAll("Zip laws", ScalazProperties.zip.laws[Future])

  checkAll("Traverse laws", traverseLaws)

  def traverseLaws: Properties = new Properties("traverse") {
    private def resizeProp(p: Prop, max: Int): Prop = new Prop {
      def apply(params: Gen.Parameters) =
        p(params.withSize(params.size % (max + 1)))
    }

    include(ScalazProperties.functor.laws[Future])
    include(ScalazProperties.foldable.laws[Future])
    property("identity traverse") = ScalazProperties.traverse.identityTraverse[Future, Int, Int]

    import std.list._, std.option._, std.stream._

    property("purity.option") = ScalazProperties.traverse.purity[Future, Option, Int]
    property("purity.stream") = ScalazProperties.traverse.purity[Future, Stream, Int]

    property("sequential fusion") =
      resizeProp(ScalazProperties.traverse.sequentialFusion[Future, Option, List, Int, Int, Int], 3)
  }
}
