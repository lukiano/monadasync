package monadasync

import cats.{ ~>, Monad }
import cats.data.Xor

import scala.concurrent.{ Future => SFuture, Promise, ExecutionContext }
import scala.util.{ Failure, Success }

object ScalaFuture {

  private val defaultContext: ExecutionContext =
    ExecutionContext.global

  implicit val ScalaFutureMonad = cats.std.future.futureInstance(defaultContext)

  def scalaFutureMonadAsync(context: ExecutionContext): MonadAsync[SFuture] =
    new MonadAsync[SFuture] {
      override def delay[A](a: => A) =
        SFuture(a)(context)

      override def suspend[A](a: => SFuture[A]) =
        delay(a).flatMap(identity)(context)

      override def now[A](a: A) =
        SFuture.successful(a)

      override def async[A](listen: Callback[A]) = {
        val p = Promise[A]()
        listen { a =>
          p.success(a)
          ()
        }
        p.future
      }
    }

  implicit val ScalaFutureMonadAsync = scalaFutureMonadAsync(defaultContext)

  /**
   * scala.Future ~> F
   * Caution: The F will most likely start computing immediately
   */
  implicit class ScalaFutureAsync[A](val f: SFuture[A]) extends AnyVal {
    def callback(implicit ec: ExecutionContext): Callback[Throwable Xor A] = { cb =>
      f onComplete {
        case Success(a) => cb(Xor.Right(a))
        case Failure(t) => cb(Xor.Left(t))
      }
    }
    def liftAsync[F[_]](implicit MA: MonadAsync[F], M: Monad[F], C: Catchable[F]): F[A] = {
      MA.async(f.callback(defaultContext)).unattempt
    }
  }
  implicit def ScalaFutureTransformation[F[_]](implicit MA: MonadAsync[F], M: Monad[F], C: Catchable[F]): SFuture ~> F =
    new (SFuture ~> F) {
      def apply[A](f: SFuture[A]): F[A] = f.liftAsync[F]
    }
}
