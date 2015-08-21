# MonadAsync

```scala
type Callback[A] = (A => Unit) => Unit

trait MonadAsync[F[_]] {
  def now[A](a: A): F[A]
  def delay[A](a: => A): F[A]
  def suspend[A](fa: => F[A]): F[A]
  def async[A](listen: Callback[A]): F[A]
  def async[A](a: => A)(implicit pool: Executor): F[A]
  def fork[A](fa: => F[A])(implicit pool: Executor): F[A]
  def mapA[A, B](fa: F[A])(f: A => B)(implicit pool: Executor): F[B]
  def bindA[A, B](fa: F[A])(f: A => F[B])(implicit pool: Executor): F[B]
  def schedule[A](a: => A, delay: Duration)(implicit pool: ScheduledExecutorService): F[A]
}

```

and MonadAsync laws that instances need to comply. 

## MonadAsync for scalaz

Because Task is just an EitherT[ Future, Throwable, ? ].

Because Task's handy operations (handle, timed, retry) are lost when you have a WriterT[ Task, ? ]

- Provides MonadAsync instances for Future, Task, EitherT, WriterT, ReaderT and StateT.
- Also Nondeterminism instances.
- Provides a Retry operation using Catchable and MonadError.
- Provides a Timer that returns a value after certain time has passed, and timeouts for effects / computations that take too long.
- For Scala 2.10 and 2.11

### How-To

Please refer to existing tests for usage examples.
