package monadasync

import java.util.concurrent.Executor

import cats.data.Xor
import org.scalacheck.{ Gen, Arbitrary }
import cats.{ Monad, Eq }
import org.scalacheck.Prop.forAll
import scala.concurrent.{ Future => SFuture }
import ScalaFuture._

abstract class MonadAsyncInstancesTests[F[_]](laws: MonadAsyncLaws[F], msLaws: MonadSuspendLaws[F]) extends MonadSuspendInstancesTests[F](msLaws) {

  def monadAsync[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], ArbF0: Arbitrary[() => A], EqFA: Eq[F[A]]): RuleSet = {
    new DefaultRuleSet(
      name = "monadasync",
      parent = Some(monadSuspend[A]),
      "async is delay" -> forAll(laws.asyncIsDelay[A] _)
    )
  }
}

object MonadAsyncInstancesTests {
  def apply[F[_]: MonadAsync: Monad: Catchable]: MonadAsyncInstancesTests[F] =
    new MonadAsyncInstancesTests[F](MonadAsyncLaws.monadAsyncLaw[F], MonadSuspendLaws.monadSuspendLaw[F]) {}
}

abstract class MonadAsyncSpec[F[_]: MonadAsync: Monad: Catchable] extends Spec {

  def run[A](f: F[A]): A

  def name: String

  implicit val pool: Executor = scala.concurrent.ExecutionContext.global

  implicit def arbitraryTC(implicit a: Arbitrary[Int]): Arbitrary[F[Int]] = Arbitrary {
    import MonadAsync.syntax._
    a.arbitrary flatMap { i =>
      Gen.oneOf(
        Gen.const(i.now[F]),
        Gen.const(delay[F, Int](i)),
        Gen.const(async[F, Int](i))
      )
    }
  }

  implicit def eqTC[A]: Eq[F[A]] = Eq.instance {
    case (f1, f2) => run(f1) == run(f2)
  }

  checkAll(name, MonadAsyncInstancesTests[F].monadAsync[Int])
}

class TaskSpec extends MonadAsyncSpec[Task] {
  import scala.concurrent.SyncVar
  def name = "Task"
  def run[A](f: Task[A]) = {
    val sync = new SyncVar[Throwable Xor A]
    f.value.runAsync(sync.put)
    sync.take().fold({t => throw t}, identity)
  }
}

class ScalaFutureSpec extends MonadAsyncSpec[SFuture] {
  import scala.concurrent.Await
  import scala.concurrent.duration._

  def name = "Scala Future"
  def run[A](f: SFuture[A]) = Await.result(f, 1 second)
}

