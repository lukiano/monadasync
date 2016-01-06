package monadasync
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ BlockingQueue, ArrayBlockingQueue, RejectedExecutionException }

import org.junit.runner.RunWith
import org.specs2.specification.core.SpecStructure

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future, Promise }
import scalaz.syntax.functor._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class AsyncSemaphoreSpec extends ImmutableSpec with ByteVectorArbitraries {

  import ScalaFuture._

  def is: SpecStructure = s2"""
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

  private class AsyncSemaphoreHelper(val sem: AsyncSemaphore[Future], c: AtomicInteger, val permits: BlockingQueue[Permit]) {
    def copy(sem: AsyncSemaphore[Future] = this.sem, count: Int = this.count, permits: BlockingQueue[Permit] = this.permits) =
      AsyncSemaphoreHelper(sem, count, permits)
    def count: Int = c.get()
    def incrementAndGet(): Int = c.incrementAndGet()
  }

  private object AsyncSemaphoreHelper {
    def apply(sem: AsyncSemaphore[Future], count: AtomicInteger, permits: BlockingQueue[Permit]): AsyncSemaphoreHelper =
      new AsyncSemaphoreHelper(sem, count, permits)
    def apply(sem: AsyncSemaphore[Future], count: Int, permits: BlockingQueue[Permit]): AsyncSemaphoreHelper =
      apply(sem, new AtomicInteger(count), permits)
    def apply(sem: AsyncSemaphore[Future], permits: BlockingQueue[Permit]): AsyncSemaphoreHelper =
      apply(sem, 0, permits)
    def apply(sem: AsyncSemaphore[Future]): AsyncSemaphoreHelper =
      apply(sem, new ArrayBlockingQueue[Permit](255, true))
  }

  private val timeout = 1.second

  private def validateConstructor = {
    def badPermits: AsyncSemaphore[Future] = { new AsyncSemaphore[Future](0) }
    def badMaxWaiters: AsyncSemaphore[Future] = { new AsyncSemaphore[Future](1, -1) }
    (badPermits must throwA[Exception]) and (badMaxWaiters must throwA[IllegalArgumentException])
  }

  private def acquire(s: AsyncSemaphoreHelper): Future[Permit] = {
    val fPermit = s.sem.acquire()
    fPermit ∘ { permit =>
      s.incrementAndGet()
      s.permits add permit
    }
    fPermit
  }

  private def executeImmediately = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)
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

  private def executeDeferred = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)
    acquire(semHelper)
    acquire(semHelper)
    acquire(semHelper)
    acquire(semHelper)

    (semHelper.count === 2) and (semHelper.sem.numPermitsAvailable === 0) and {
      semHelper.permits.take().release()
      semHelper.count === 3
    } and {
      semHelper.permits.take().release()
      semHelper.count === 4
    } and {
      semHelper.permits.take().release()
      semHelper.count === 4
    }
  }

  private def boundWaiters = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)
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
      semHelper2.permits.take().release()
      semHelper2.permits.take().release()
      semHelper2.permits.take().release()
      semHelper2.count === 5
    }
  }

  private def exceptions = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)

    val p1 = acquire(semHelper)
    val p2 = acquire(semHelper)
    val p3 = acquire(semHelper)

    p3 >| new Exception("OK")
    def waitPermit: Permit = { Await.result(p3, timeout) }
    (waitPermit must throwA[Exception]) and {
      Await.result(p2, timeout).release()
      Await.result(p1, timeout).release()
      true must beTrue
    }
  }

  private def executeQueue = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)

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

  private def releasePermitException = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)

    val badFunc = new (() => Future[Unit]) {
      def apply(): Future[Unit] = throw new RuntimeException("bad func calling")
    }
    semHelper.sem.acquireAndRun(badFunc())
    semHelper.sem.numPermitsAvailable === 2
  }

  private def executeSyncQueue = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)

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

  private def handleSyncException = {
    val sem = new AsyncSemaphore[Future](2)
    val semHelper = AsyncSemaphoreHelper(sem)

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

  private def mutex = {
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