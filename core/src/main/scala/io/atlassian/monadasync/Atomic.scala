package io.atlassian.monadasync

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference

import scalaz.{ -\/, \/-, Catchable, Monad }

trait Atomic[F[_], A] {
  def get: F[Option[A]]
  def getOrElse(a: => A): F[A]
  def set: A => F[Unit]
  def getOrSet(fa: => F[A]): F[A]
}

object Atomic {
  import scalaz.syntax.monad._
  import MonadAsync.syntax._
  def basic[F[_]: Monad, A]: Atomic[F, A] = new Atomic[F, A] {
    private val value = new AtomicReference[Option[A]](None)
    override def get = value.get.point
    override def getOrElse(a: => A) = get map { _.getOrElse(a) }
    override def set = a => value.set(Some(a)).point
    override def getOrSet(fa: => F[A]): F[A] = get >>= {
      case Some(a) => a.point[F]
      case None => fa >>= { a =>
        set(a) >| a
      }
    }
  }

  def delayed[F[_], A](implicit MA: MonadSuspend[F]): Atomic[F, A] = new Atomic[F, A] {
    implicit val monad = MA.monad
    private val value = new AtomicReference[Option[A]](None)
    override def get = MA.delay { value.get }
    override def getOrElse(a: => A) = get map { _.getOrElse(a) }
    override def set = a => MA.delay { value.set(Some(a)) }
    override def getOrSet(fa: => F[A]): F[A] = get >>= {
      case Some(a) => a.point[F]
      case None => fa >>= { a =>
        set(a) >| a
      }
    }
  }

  def synchronized[F[_]: Monad: MonadAsync: Catchable, A]: Atomic[F, A] = new Atomic[F, A] {
    private val mutex = new AsyncMutex[F]()
    @volatile private var value: Option[A] = None

    override def get = MonadSuspend[F].suspend {
      mutex.acquire() map { permit =>
        val v = value
        permit.release()
        v
      }
    }

    override def getOrElse(a: => A) = get map {
      _.getOrElse(a)
    }

    override def set = a =>
      mutex.acquire() map { permit =>
        value = Some(a)
        permit.release()
      }

    override def getOrSet(fa: => F[A]) = MonadSuspend[F].suspend {
      mutex.acquire() flatMap { permit =>
        value match {
          case Some(a) =>
            permit.release()
            a.now
          case None =>
            Catchable[F].attempt(fa) flatMap {
              case \/-(a) =>
                value = Some(a)
                permit.release()
                a.now
              case -\/(t) =>
                permit.release()
                Catchable[F].fail(t)
            }
        }
      }
    }
  }

  def forked[F[_], A](under: Atomic[F, A])(implicit MA: MonadAsync[F], executor: Executor): Atomic[F, A] = new Atomic[F, A] {
    override def get = MA.fork { under.get }
    override def getOrElse(a: => A) = MA.fork { under.getOrElse(a) }
    override def set = a => MA.fork { under.set(a) }
    override def getOrSet(fa: => F[A]) = MA.fork { under.getOrSet(fa) }
  }
}