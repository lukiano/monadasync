package monadasync
import scalaz._
import scalaz.scalacheck.ScalazProperties
import scalaz.std.anyVal._
import scalaz.concurrent.Future
import Future._
import MonadAsync._
import Nondeterminisms._
import Catchables._

object Type {
  type Task[A] = EitherT[Future, Throwable, A]
  type WrittenTask[A] = WriterT[Task, Int, A]
  type F[A] = ReaderT[WrittenTask, Unit, A]
}

object ComplexSpec extends MonadAsyncSpec[Type.F] {

  override def run[A](f: Type.F[A]): A =
    f.run(()).value.run.run.toOption.get

  override val laws = MonadAsyncProperties.monadAsync.laws[Type.F](
    MonadAsync[Type.F],
    Nondeterminism[Type.F],
    Catchable[Type.F],
    arbitraryTC,
    arbitraryInt,
    arbitraryF0,
    arbitraryF1,
    equalTc
  )

  checkAll("MonadAsync laws", laws)

  checkAll("Monad laws", ScalazProperties.monad.laws[Type.F])
}
