package monadasync

import MonadAsync.syntax._

import scala.concurrent.duration.Duration
import scalaz._
import scalaz.syntax.monad._

object Retry {

  abstract class RetryOps[F[_]: MonadAsync: Monad, L, A](self: F[A]) {
    /**
     * Retries this F if it fails, once for each element in `delays`,
     * each retry delayed by the corresponding duration, accumulating
     * left sides into a list.
     * A retriable failure is one for which the predicate `p` returns `true`.
     */
    def retryAccumulating(delays: Seq[Duration], p: L => Boolean): F[(A, List[L])] =
      retryInternal(delays, p, accumulateErrors = true)

    /**
     * Retries this F if it fails, once for each element in `delays`,
     * each retry delayed by the corresponding duration.
     * A retriable failure is one for which the predicate `p` returns `true`.
     */
    def retry(delays: Seq[Duration], p: L => Boolean): F[A] =
      retryInternal(delays, p, accumulateErrors = false) map { _._1 }

    protected def retryLogic(
      f: F[(A, List[L])],
      t: Duration,
      ts: Seq[Duration],
      es: => Stream[L],
      p: L => Boolean,
      accumulateErrors: Boolean
    ): F[(A, List[L])]

    protected final def loop(
      ds: Seq[Duration],
      es: => Stream[L],
      p: L => Boolean,
      accumulateErrors: Boolean
    ): F[(A, List[L])] = {
      def acc = if (accumulateErrors) es.toList else Nil
      ds match {
        case Seq() => self map (_ -> acc)
        case Seq(t, ts @ _*) => retryLogic(self map (_ -> acc), t, ts, es, p, accumulateErrors)
      }
    }

    private def retryInternal(
      delays: Seq[Duration],
      p: L => Boolean,
      accumulateErrors: Boolean
    ): F[(A, List[L])] =
      loop(delays, Stream(), p, accumulateErrors)
  }

  object catchable {
    implicit def ToRetryOps[F[_]: MonadAsync: Monad: Catchable, A](self: F[A]): RetryOps[F, Throwable, A] =
      new RetryOps[F, Throwable, A](self) {
        import scalaz.syntax.catchable._
        protected override def retryLogic(
          f: F[(A, List[Throwable])],
          t: Duration,
          ts: Seq[Duration],
          es: => Stream[Throwable],
          p: Throwable => Boolean,
          accumulateErrors: Boolean
        ): F[(A, List[Throwable])] =
          f.attempt.flatMap {
            case \/-(v) => v.point[F]
            case -\/(e) if p(e) =>
              loop(ts, e #:: es, p, accumulateErrors) after t
            case -\/(e) => Catchable[F].fail(e)
          }

      }
  }
}
