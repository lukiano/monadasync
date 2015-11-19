package io.atlassian.monadasync

import java.util.concurrent.{ Executors, ThreadFactory }

import scala.collection.immutable.Queue
import scala.concurrent.SyncVar
import scalaz.\/
import scalaz.concurrent.Task

// borrowed from scalaz concurrent tests
object ConcurrentTaskSpec extends org.specs2.mutable.SpecificationWithJUnit {

  type TC[A] = Task[A]
  val MA = MonadAsync[TC]
  implicit val pool = DefaultExecutor
  import MA._

  "Task" should {

    "correctly use threads when forked and flatmapped" in {
      @volatile var q = Queue[(Int, String)]()

      val forked = "forked-thread"
      val current = Thread.currentThread().getName

      def enqueue(taskId: Int) =
        q = q.enqueue((taskId, Thread.currentThread().getName))

      val es = Executors.newFixedThreadPool(1, new ThreadFactory {
        def newThread(p1: Runnable) = new Thread(p1, forked)
      })

      val sync = new SyncVar[Boolean]

      (for {
        _ <- now(enqueue(1))
        _ <- delay(enqueue(2))
        _ <- fork(now(enqueue(3)))(es)
        _ <- delay(enqueue(4))
        _ <- fork(now(enqueue(5)))(es)
        _ <- now(enqueue(6))
        _ <- fork(delay(enqueue(7)))(es)

      } yield ()).runAsync(_ => {
        enqueue(8)
        sync.put(true)
      })
      enqueue(9)

      sync.get(5000) must_== Some(true)

      val runned = q.toList

      //trampoline should be evaluated at the head before anything else gets evaluated
      runned(0) must_== ((1, current))
      runned(1) must_== ((2, current))

      //the after async must not be the last ever
      (runned.last._1 != 9) must_== (true)

      //the rest of tasks must be run off the forked thread
      runned.filter(_._2 == forked).map(_._1) must_== List(3, 4, 5, 6, 7, 8)

    }

    "complete even when interrupted" in {
      val t = fork(delay(Thread.sleep(3000)))
      val sync = new SyncVar[Throwable \/ Unit]
      val interrupt = t.runAsyncInterruptibly(sync.put)
      Thread.sleep(1000)
      interrupt()
      sync.get(3000) map { _.toEither } must beSome {
        e: Either[Throwable, Unit] =>
          e must beLeft {
            t: Throwable => t === Task.TaskInterrupted
          }
      }
    }

  }
}