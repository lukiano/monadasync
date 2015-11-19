package io.atlassian.monadasync

import scalaz.stream.Process._
import scalaz.stream._

package object stream {

  /** Syntax for Sink */
  implicit class SinkSyntax[F[_]: MonadSuspend, I](val self: Sink[F, I]) {
    implicit val monad = MonadAsync[F].monad

    /** converts sink to sink that first pipes received `I0` to supplied p1 */
    def pipeIn[I0](p1: Process1[I0, I]): Sink[F, I0] = suspend {
      import scalaz.Scalaz._
      // Note: Function `f` from sink `self` may be used for more than 1 element emitted by `p1`.
      @volatile var cur = p1.step
      @volatile var lastF: Option[I => F[Unit]] = None
      self.takeWhile { _ =>
        cur match {
          case Halt(Cause.End) => false
          case Halt(cause)     => throw new Cause.Terminated(cause)
          case _               => true
        }
      } map { (f: I => F[Unit]) =>
        lastF = f.some
        (i0: I0) => MonadSuspend[F].suspend {
          cur match {
            case Halt(_) => sys.error("Impossible")
            case Step(Emit(piped), cont) =>
              cur = process1.feed1(i0) { cont.continue }.step
              piped.toList.traverse_(f)
            case Step(hd, cont) =>
              val (piped, tl) = process1.feed1(i0)(hd +: cont).unemit
              cur = tl.step
              piped.toList.traverse_(f)
          }
        }
      } onHalt {
        case Cause.Kill =>
          lastF map { f =>
            cur match {
              case Halt(_) => sys.error("Impossible (2)")
              case s @ Step(_, _) =>
                s.toProcess.disconnect(Cause.Kill).evalMap(f).drain
            }
          } getOrElse Halt(Cause.Kill)
        case Cause.End          => halt
        case c @ Cause.Error(_) => halt.causedBy(c)
      }
    }
  }

}
