package io.atlassian.monadasync

import java.util.concurrent.{ ForkJoinPool, Executor, CompletableFuture }
import java.util.function.{ Function => JFunction, BiFunction => JBiFunction, Supplier => JSupplier, Consumer => JConsumer, BiConsumer => JBiConsumer }

import scala.util.{ Failure, Success, Try }
import scalaz._
import scalaz.syntax.id._

object JavaFuture {

  private val defaultExecutor: Executor = ForkJoinPool.commonPool()

  implicit val CompletableFutureMonad = completableFutureMonad(defaultExecutor)

  // MonadError doesn't hold laws as CompletableFuture wraps Exceptions on each step.
  def completableFutureMonad(executor: Executor): Monad[CompletableFuture] with Nondeterminism[CompletableFuture] with Comonad[CompletableFuture] with MonadError[λ[(?, α) => CompletableFuture[α]], Throwable] with Zip[CompletableFuture] =
    new Monad[CompletableFuture] with Nondeterminism[CompletableFuture] with Comonad[CompletableFuture] with MonadError[λ[(?, α) => CompletableFuture[α]], Throwable] with Zip[CompletableFuture] {
      override def chooseAny[A](head: CompletableFuture[A], tail: Seq[CompletableFuture[A]]): CompletableFuture[(A, Seq[CompletableFuture[A]])] = {
        val p = new CompletableFuture[(Try[A], CompletableFuture[A])]()
        val result = new CompletableFuture[(A, Seq[CompletableFuture[A]])]()
        val all: Seq[CompletableFuture[A]] = head +: tail
        all foreach { f =>
          f.whenCompleteAsync(new JBiConsumer[A, Throwable] {
            //            p.trySuccess((t, f)))(context)
            override def accept(a: A, t: Throwable): Unit = {
              Option(t) match {
                case None => p.complete((Success(a), f))
                case Some(e) => p.complete((Failure(e), f))
              }
              ()
            }
          }, executor)
        }
        p.thenAcceptAsync(new JConsumer[(Try[A], CompletableFuture[A])] {
          override def accept(tuple: (Try[A], CompletableFuture[A])): Unit = {
            tuple._1 match {
              case Success(a) => result.complete((a, all.filter(_ != tuple._2)))
              case Failure(t) => result.completeExceptionally(t)
            }
            ()
          }
        }, executor)
        result
      }

      override def copoint[A](p: CompletableFuture[A]): A =
        p.get()

      override def point[A](a: => A): CompletableFuture[A] =
        CompletableFuture.supplyAsync(new JSupplier[A] {
          override def get(): A =
            a
        }, executor)

      override def cobind[A, B](fa: CompletableFuture[A])(f: CompletableFuture[A] => B): CompletableFuture[B] =
        CompletableFuture.completedFuture(f(fa))

      override def bind[A, B](fa: CompletableFuture[A])(f: A => CompletableFuture[B]): CompletableFuture[B] =
        fa.thenComposeAsync(new JFunction[A, CompletableFuture[B]] {
          override def apply(a: A): CompletableFuture[B] =
            f(a)
        }, executor)

      override def map[A, B](fa: CompletableFuture[A])(f: A => B): CompletableFuture[B] =
        fa.thenApplyAsync(new JFunction[A, B] {
          override def apply(a: A): B =
            f(a)
        }, executor)

      override def raiseError[A](e: Throwable): CompletableFuture[A] =
        new CompletableFuture[A] <| { _.completeExceptionally(e) }

      override def handleError[A](fa: CompletableFuture[A])(f: Throwable => CompletableFuture[A]): CompletableFuture[A] = {
        val ffa: CompletableFuture[CompletableFuture[A]] = fa.handleAsync(new JBiFunction[A, Throwable, CompletableFuture[A]] {
          override def apply(a: A, t: Throwable) =
            Option(t) match {
              case None => fa
              case Some(e) => f(e)
            }
        }, executor)
        ffa.thenCompose(new JFunction[CompletableFuture[A], CompletableFuture[A]] {
          override def apply(fa: CompletableFuture[A]): CompletableFuture[A] =
            fa
        })
      }

      override def zip[A, B](a: => CompletableFuture[A], b: => CompletableFuture[B]): CompletableFuture[(A, B)] =
        a.thenCombineAsync(b, new JBiFunction[A, B, (A, B)] {
          override def apply(a: A, b: B) =
            (a, b)
        }, executor)
    }

  implicit val CompletableFutureCatchable = completableFutureCatchable(defaultExecutor)

  def completableFutureCatchable(executor: Executor): Catchable[CompletableFuture] = new Catchable[CompletableFuture] {
    override def attempt[A](f: CompletableFuture[A]): CompletableFuture[Throwable \/ A] =
      f.handleAsync(new JBiFunction[A, Throwable, Throwable \/ A] {
        override def apply(a: A, t: Throwable) =
          Option(t) match {
            case None => \/-(a)
            case Some(e) => -\/(e)
          }
      }, executor)

    override def fail[A](err: Throwable): CompletableFuture[A] =
      new CompletableFuture[A] <| { _.completeExceptionally(err) }
  }

  implicit val CompletableFutureMonadAsync = completableFutureMonadAsync(defaultExecutor)

  def completableFutureMonadAsync(executor: Executor): MonadAsync[CompletableFuture] = new MonadAsync[CompletableFuture] {
    override def async[A](listen: Callback[A]): CompletableFuture[A] =
      new CompletableFuture[A] <| { p =>
        listen { a =>
          p.complete(a)
          ()
        }
      }

    override implicit def monad: Monad[CompletableFuture] =
      CompletableFutureMonad

    override def now[A](a: A): CompletableFuture[A] =
      CompletableFuture.completedFuture(a)

    override def delay[A](a: => A): CompletableFuture[A] =
      CompletableFuture.supplyAsync(new JSupplier[A] {
        override def get(): A =
          a
      }, executor)
  }
}
