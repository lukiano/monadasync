package monadasync.internal

/**
 * Provides asynchronous operations for F
 */
private[monadasync] trait MonadSuspend[F[_]] {
  /**
   * @return an F whose value is immediately set.
   */
  def now[A](a: A): F[A]

  /**
   * @return an F whose value will be eventually computed.
   */
  def delay[A](a: => A): F[A]

  /**
   * @return an F[A] wrapped in a suspension that will be eventually computed.
   */
  def suspend[A](fa: => F[A]): F[A]
}
