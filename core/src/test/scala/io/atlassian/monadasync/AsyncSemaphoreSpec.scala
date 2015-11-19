package io.atlassian.monadasync

import java.util.concurrent.{ ConcurrentLinkedQueue, RejectedExecutionException }

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future, Promise, duration }

object AsyncSemaphoreSpec {

  implicit val context = ExecutionContext.fromExecutor(MonadAsync.SameThreadExecutor)

  implicit object FutureMonadAsync extends MonadAsync[Future] {
    override def now[A](a: A) = Future.successful(a)
    override protected implicit def M: Monad[Future] = Monad[Future]
    override def async[A](listen: Callback[A]) = {
      val p = Promise[A]()
      listen { a =>
        p.success(a)
        ()
      }
      p.future
    }
  }

  implicit object FutureCatchable extends Catchable[Future] {
    override def attempt[A](f: Future[A]) = f map { a => \/-(a) } recover { case t => -\/(t) }
    override def fail[A](err: Throwable) = Future.failed[A](err)
  }

}

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class AsyncSemaphoreSpec extends BlobstoreSpec with ByteVectorArbitraries {
  import AsyncSemaphoreSpec._

  def is = s2"""
    This is a specification to check AsyncSemaphore and AsyncMutex

    AsyncSemaphore should
      validate constructor parameters                               $validateConstructor
      execute immediately while permits are available               $executeImmediately
      execute deferred computations when permits are released       $executeDeferred
      bound the number of waiters                                   $boundWaiters
      satisfy futures with exceptions if they are interrupted       $exceptions
      execute queued up async functions as permits become available $executeQueue
      release permit even if queued up function throws an exception $releasePermitException
      execute queued up sync functions as permits become available  $executeSyncQueue
      handle queued up sync functions which throw exception         $handleSyncException

    AsyncMutex should
      admit only one operation at a time $mutex
  """

  private class AsyncSemaphoreHelper(val sem: AsyncSemaphore[Future], var count: Int, val permits: ConcurrentLinkedQueue[Permit]) {
    def copy(sem: AsyncSemaphore[Future] = this.sem, count: Int = this.count, permits: ConcurrentLinkedQueue[Permit] = this.permits) =
      new AsyncSemaphoreHelper(sem, count, permits)
  }

  val timeout = 1.second

  def validateConstructor = {
    def badPermits: AsyncSemaphore[Future] = { new AsyncSemaphore[Future](0) }
    def badMaxWaiters: AsyncSemaphore[Future] = { new AsyncSemaphore[Future](1, -1) }
    (badPermits must throwA[Exception]) and (badMaxWaiters must throwA[IllegalArgumentException])
  }

  private def acquire(s: AsyncSemaphoreHelper): Future[Permit] = {
    val fPermit = s.sem.acquire()
    fPermit map { permit =>
      s.count += 1
      s.permits add permit
    }
    fPermit
  }

  def executeImmediately = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])
    (semHelper.sem.numPermitsAvailable === 2) and {
      acquire(semHelper)
      (semHelper.count === 1) and (semHelper.sem.numPermitsAvailable === 1)
    } and {
      acquire(semHelper)
      (semHelper.count === 2) and (semHelper.sem.numPermitsAvailable === 0)
    } and {
      acquire(semHelper)
      (semHelper.count === 2) and (semHelper.sem.numPermitsAvailable === 0)
    }
  }

  def executeDeferred = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])
    acquire(semHelper)
    acquire(semHelper)
    acquire(semHelper)
    acquire(semHelper)

    (semHelper.count === 2) and (semHelper.sem.numPermitsAvailable === 0) and {
      semHelper.permits.poll().release()
      semHelper.count === 3
    } and {
      semHelper.permits.poll().release()
      semHelper.count === 4
    } and {
      semHelper.permits.poll().release()
      semHelper.count === 4
    }
  }

  def boundWaiters = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])
    val semHelper2 = semHelper.copy(sem = new AsyncSemaphore[Future](2, 3))

    // The first two acquires obtain a permit.
    acquire(semHelper2)
    acquire(semHelper2)

    (semHelper2.count === 2) and {
      // The next three acquires wait.
      acquire(semHelper2)
      acquire(semHelper2)
      acquire(semHelper2)

      (semHelper2.count === 2) and (semHelper2.sem.numWaiters === 3)
    } and {
      // The next acquire should be rejected.
      val permit = acquire(semHelper2)
      def waitPermit: Permit = { Await.result(permit, timeout) }
      (semHelper2.sem.numWaiters === 3) and (waitPermit must throwA[RejectedExecutionException])
    } and {
      // Waiting Futures should still execute once permits are available.
      semHelper2.permits.poll().release()
      semHelper2.permits.poll().release()
      semHelper2.permits.poll().release()
      semHelper2.count === 5
    }
  }

  def exceptions = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])

    val p1 = acquire(semHelper)
    val p2 = acquire(semHelper)
    val p3 = acquire(semHelper)

    p3.map(_ => new Exception("OK"))
    def waitPermit: Permit = { Await.result(p3, timeout) }
    (waitPermit must throwA[Exception]) and {
      Await.result(p2, timeout).release()
      Await.result(p1, timeout).release()
      true must beTrue
    }
  }

  def executeQueue = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])

    var counter = 0
    val queue = new scala.collection.mutable.Queue[Promise[Unit]]()
    val func = new (() => Future[Unit]) {
      def apply(): Future[Unit] = {
        counter = counter + 1
        val promise = Promise[Unit]()
        queue.enqueue(promise)
        promise.future
      }
    }
    (semHelper.sem.numPermitsAvailable === 2) and {
      semHelper.sem.acquireAndRun(func())
      counter === 1
    } and (semHelper.sem.numPermitsAvailable === 1) and {
      semHelper.sem.acquireAndRun(func())
      counter === 2
    } and (semHelper.sem.numPermitsAvailable === 0) and {
      semHelper.sem.acquireAndRun(func())
      counter === 2
    } and (semHelper.sem.numPermitsAvailable === 0) and {
      queue.dequeue().success(())
      counter === 3
    } and (semHelper.sem.numPermitsAvailable === 0) and {
      queue.dequeue().success(())
      semHelper.sem.numPermitsAvailable === 1
    } and {
      queue.dequeue().failure(new RuntimeException("test"))
      semHelper.sem.numPermitsAvailable === 2
    }
  }

  def releasePermitException = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])

    val badFunc = new (() => Future[Unit]) {
      def apply(): Future[Unit] = throw new RuntimeException("bad func calling")
    }
    semHelper.sem.acquireAndRun(badFunc())
    semHelper.sem.numPermitsAvailable === 2
  }

  def executeSyncQueue = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])

    var counter = 0
    val queue = new scala.collection.mutable.Queue[Promise[Unit]]()
    val funcFuture = new (() => Future[Unit]) {
      def apply(): Future[Unit] = {
        counter = counter + 1
        val promise = Promise[Unit]()
        queue.enqueue(promise)
        promise.future
      }
    }
    val func = new (() => Int) {
      def apply(): Int = {
        counter = counter + 1
        counter
      }
    }
    (semHelper.sem.numPermitsAvailable === 2) and {
      semHelper.sem.acquireAndRun(funcFuture())
      counter === 1
    } and (semHelper.sem.numPermitsAvailable === 1) and {
      semHelper.sem.acquireAndRun(funcFuture())
      counter === 2
    } and (semHelper.sem.numPermitsAvailable === 0) and {
      val future = semHelper.sem.acquireAndRunSync(func())
      (counter === 2) and (semHelper.sem.numPermitsAvailable === 0) and {
        // sync func is blocked at this point.
        // But it should be executed as soon as one of the queued up future functions finish
        queue.dequeue().success(())
        counter === 3
      } and {
        val result = Await.result(future, timeout)
        (result === 3) and (semHelper.sem.numPermitsAvailable === 1)
      }
    }
  }

  def handleSyncException = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = new AsyncSemaphoreHelper(sem, 0, new ConcurrentLinkedQueue[Permit])

    var counter = 0
    val queue = new scala.collection.mutable.Queue[Promise[Unit]]()
    val funcFuture = new (() => Future[Unit]) {
      def apply(): Future[Unit] = {
        counter = counter + 1
        val promise = Promise[Unit]()
        queue.enqueue(promise)
        promise.future
      }
    }
    val badFunc = new (() => Unit) {
      def apply(): Unit = {
        throw new Exception("error!")
      }
    }
    (semHelper.sem.numPermitsAvailable === 2) and {
      semHelper.sem.acquireAndRun(funcFuture())
      counter === 1
    } and (semHelper.sem.numPermitsAvailable === 1) and {
      semHelper.sem.acquireAndRun(funcFuture())
      counter === 2
    } and (semHelper.sem.numPermitsAvailable === 0) and {
      val future = semHelper.sem.acquireAndRunSync(badFunc())
      (counter === 2) and (semHelper.sem.numPermitsAvailable === 0) and {
        // sync func is blocked at this point.
        // But it should be executed as soon as one of the queued up future functions finish

        queue.dequeue().success(())
        def waitForFuture(): Unit = {
          Await.result(future, timeout)
        }
        (counter === 2) and (waitForFuture() must throwA[Exception]) and (semHelper.sem.numPermitsAvailable === 1)
      }
    }
  }

  implicit class IsDefined[A](val t: Future[A]) {
    def isDefined: Boolean = t.isCompleted
  }

  def mutex = {
    val m = new AsyncMutex[Future]
    val a0 = m.acquire()
    val a1 = m.acquire()
    (a0.isDefined === true) and (a1.isDefined === false) and {
      Await.result(a0, timeout).release() // satisfy operation 0
      a1.isDefined === true // 1 now available
    } and {
      val a2 = m.acquire()
      (a2.isDefined === false) and {
        Await.result(a1, timeout).release() // satisfy operation 1
        a2.isDefined === true
      }
    }
  }
}