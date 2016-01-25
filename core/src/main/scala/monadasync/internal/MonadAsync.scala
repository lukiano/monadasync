package monadasync.internal

import java.util.concurrent.Executor

/**
 * Provides asynchronous operations for F
 */
private[monadasync] trait MonadAsync[F[_]] extends MonadSuspend[F] {

  /**
   * @return an F whose value will be set from an asynchronous computation, via callback.
   */
  def async[A](listen: (A => Unit) => Unit): F[A]

  def async[A](pool: Executor)(a: => A): F[A]
}
