package monadasync
import org.scalacheck.{ Properties, Arbitrary }
import org.scalacheck.Prop._

import scalaz._

object MonadAsyncProperties {
  object monadAsync {
    def asyncIsDelay[F[_], A](implicit ma: MonadAsync[F], m: Monad[F], c: Catchable[F], eq: Equal[F[A]], a0: Arbitrary[() => A]) =
      forAll(MonadAsync.monadAsyncLaw[F](ma).asyncIsDelay[A] _)

    def laws[M[_]](implicit ma: MonadAsync[M], m: Monad[M], c: Catchable[M], am: Arbitrary[M[Int]], a: Arbitrary[Int], am0: Arbitrary[() => Int],
      ama: Arbitrary[Int => M[Int]], e: Equal[M[Int]]) =
      new Properties("monad async") {
        property("suspendIsDelayJoin") = monadSuspend.suspendIsDelayJoin[M, Int]
        property("nowIsPoint") = monadSuspend.nowIsPoint[M, Int]
        property("delayIsMap") = monadSuspend.delayIsMap[M, Int]
        property("asyncIsDelay") = asyncIsDelay[M, Int]
      }
  }

  object monadSuspend {
    def suspendIsDelayJoin[F[_], A](implicit ma: MonadSuspend[F], m: Monad[F], eq: Equal[F[A]], a0: Arbitrary[F[A]]) =
      forAll(MonadSuspend.monadSuspendLaw[F](ma).suspendIsDelayJoin[A] _)
    def nowIsPoint[F[_], A](implicit ma: MonadSuspend[F], m: Monad[F], eq: Equal[F[A]], a0: Arbitrary[A]) =
      forAll(MonadSuspend.monadSuspendLaw[F](ma).nowIsPoint[A] _)
    def delayIsMap[F[_], A](implicit ma: MonadSuspend[F], m: Monad[F], eq: Equal[F[A]], a0: Arbitrary[() => A]) =
      forAll(MonadSuspend.monadSuspendLaw[F](ma).delayIsMap[A] _)

    def laws[M[_]](implicit ma: MonadSuspend[M], m: Monad[M], am: Arbitrary[M[Int]], a: Arbitrary[Int], am0: Arbitrary[() => Int],
      ama: Arbitrary[Int => M[Int]], e: Equal[M[Int]]) =
      new Properties("monad async") {
        property("suspendIsDelayJoin") = suspendIsDelayJoin[M, Int]
        property("nowIsPoint") = nowIsPoint[M, Int]
        property("delayIsMap") = delayIsMap[M, Int]
      }
  }
}
