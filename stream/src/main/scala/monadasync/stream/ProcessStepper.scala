package monadasync
package stream

import java.util.concurrent.atomic.AtomicReference

import scalaz.std.option.none
import scalaz.stream.Process
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._
import scalaz.{ Catchable, Monad, Monoid }

class ProcessStepper[F[_]: Monad: Catchable, A: Monoid](p: Process[F, A]) {
  import scalaz.stream.Cause._
  import scalaz.stream.Process.{ Await, Emit, Halt, Step }

  private val cur = new AtomicReference[Process[F, A]](p)

  def read: F[Option[A]] = readFrom(finishing = true)

  val Done: F[Option[A]] = none[A].point[F]

  private def readFrom(finishing: Boolean): F[Option[A]] = {
    cur.get.step match {
      case s: Step[F, A] @unchecked =>
        (s.head, s.next) match {
          case (Emit(os), cont) =>
            os.foldLeft[A](∅)((a, o) => a |+| o).point[F] >>= { a =>
              cur.set(cont.continue)
              a.some.point[F]
            }
          case (awt: Await[F, Any, A] @unchecked, cont) =>
            awt.evaluate flatMap {
              q =>
                cur.set(q +: cont)
                readFrom(finishing = false)
            }
        }
      case Halt(End) =>
        Done
      case Halt(Kill) =>
        Done
      case Halt(Error(rsn)) =>
        Catchable[F].fail(rsn)
    }
  }
}
