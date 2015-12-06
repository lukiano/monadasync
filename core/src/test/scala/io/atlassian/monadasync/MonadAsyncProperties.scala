package io.atlassian.monadasync

import org.scalacheck.{ Properties, Arbitrary }
import org.scalacheck.Prop._

import scalaz.Equal

object MonadAsyncProperties {
  object monadAsync {
    def asyncIsDelay[F[_], A](implicit ma: MonadAsync[F], eq: Equal[F[A]], a0: Arbitrary[() => A]) =
      forAll(ma.monadAsyncLaw.asyncIsDelay[A] _)
    def bindAIsBind[F[_], A, B](implicit ma: MonadAsync[F], eq: Equal[F[B]], a: Arbitrary[A], afa: Arbitrary[A => F[A]], afb: Arbitrary[A => F[B]]) =
      forAll(ma.monadAsyncLaw.bindAIsBind[A, B] _)
    def mapAIsMap[F[_], A, B](implicit ma: MonadAsync[F], eq: Equal[F[B]], a: Arbitrary[A], afa: Arbitrary[A => F[A]], ab: Arbitrary[A => B]) =
      forAll(ma.monadAsyncLaw.mapAIsMap[A, B] _)

    def laws[M[_]](implicit ma: MonadAsync[M], am: Arbitrary[M[Int]], a: Arbitrary[Int], am0: Arbitrary[() => Int],
      ama: Arbitrary[Int => M[Int]], e: Equal[M[Int]]) =
      new Properties("monad async") {
        property("suspendIsDelayJoin") = monadSuspend.suspendIsDelayJoin[M, Int]
        property("nowIsPoint") = monadSuspend.nowIsPoint[M, Int]
        property("delayIsMap") = monadSuspend.delayIsMap[M, Int]
        property("asyncIsDelay") = asyncIsDelay[M, Int]
        property("bindAIsBind") = bindAIsBind[M, Int, Int]
        property("mapAIsMap") = mapAIsMap[M, Int, Int]
      }
  }

  object monadSuspend {
    def suspendIsDelayJoin[F[_], A](implicit ma: MonadSuspend[F], eq: Equal[F[A]], a0: Arbitrary[F[A]]) =
      forAll(ma.monadSuspendLaw.suspendIsDelayJoin[A] _)
    def nowIsPoint[F[_], A](implicit ma: MonadSuspend[F], eq: Equal[F[A]], a0: Arbitrary[A]) =
      forAll(ma.monadSuspendLaw.nowIsPoint[A] _)
    def delayIsMap[F[_], A](implicit ma: MonadSuspend[F], eq: Equal[F[A]], a0: Arbitrary[() => A]) =
      forAll(ma.monadSuspendLaw.delayIsMap[A] _)

    def laws[M[_]](implicit ma: MonadSuspend[M], am: Arbitrary[M[Int]], a: Arbitrary[Int], am0: Arbitrary[() => Int],
      ama: Arbitrary[Int => M[Int]], e: Equal[M[Int]]) =
      new Properties("monad async") {
        property("suspendIsDelayJoin") = suspendIsDelayJoin[M, Int]
        property("nowIsPoint") = nowIsPoint[M, Int]
        property("delayIsMap") = delayIsMap[M, Int]
      }
  }
}
