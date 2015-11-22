package io.atlassian.monadasync
package stream

import java.io.{ IOException, InputStream }
import java.nio.channels.{ AsynchronousFileChannel, CompletionHandler, FileChannel }
import java.nio.file.{ Files, Path, StandardOpenOption }
import java.util.concurrent.atomic.AtomicLong

import MonadSuspend.syntax._
import scodec.bits.{ BitVector, ByteVector }
import scodec.stream.StreamCodec
import scodec.stream.decode.{ Cursor, DecodingError }

import scala.annotation.tailrec
import scalaz.stream.Process._
import scalaz.stream.io.resource
import scalaz.stream.{ Cause, Channel, Process, Sink }
import scalaz.{ -\/, Catchable, Id, Monad, \/, \/-, ~> }

package object file {

  val bufferSize = 32 * 1024

  def toInputStream[F[_]: Catchable](p: Process[F, ByteVector])(implicit runner: F ~> Id.Id): InputStream = new InputStream {
    import Cause.{ EarlyCause, End, Kill }

    var cur = p

    var index = 0
    var chunks: Seq[ByteVector] = Nil // we only consider the head to be valid at any point in time

    def Try[A](p: => Process[F, A]): Process[F, A] =
      try p
      catch { case e: Throwable => Process.fail(e) }

    def read(): Int = {
      if (cur.isHalt && chunks.isEmpty) {
        -1
      } else {
        val buffer = new Array[Byte](1)
        val bytesRead = read(buffer)
        if (bytesRead == -1) {
          -1
        } else {
          buffer(0) & 0xff
        }
      }
    }

    override def read(buffer: Array[Byte], offset: Int, length: Int): Int = {
      if (cur.isHalt && chunks.isEmpty) {
        -1
      } else {
        // when our index walks off the end of our last chunk, we need to Nil it out!
        if (chunks.isEmpty) {
          step()
          read(buffer, offset, length)
        } else {
          @tailrec
          def go(offset: Int, length: Int, read: Int): Int = {
            if (chunks.isEmpty) {
              // we already took care of the "halted at start" stillborn case, so we can safely just step
              step()

              if (cur.isHalt && chunks.isEmpty)
                read // whoops! we walked off the end of the stream and we're done
              else
                go(offset, length, read)
            } else {
              val chunk = chunks.head
              val remaining = chunk.length - index

              if (length <= remaining) {
                chunk.copyToArray(buffer, offset, index, length)

                if (length == remaining) {
                  index = 0
                  chunks = chunks.tail
                } else {
                  index += length
                }

                length + read
              } else {
                chunk.copyToArray(buffer, offset, index, remaining)

                chunks = chunks.tail
                go(offset + remaining, length - remaining, read + remaining)
              }
            }
          }

          go(offset, length, 0)
        }
      }
    }

    @tailrec
    override def close() {
      if (cur.isHalt && chunks.isEmpty) {
        chunks = Nil
      } else {
        cur = cur.kill
        val haltOrStep: HaltOrStep[F, ByteVector] = cur.step
        haltOrStep match {
          case Halt(cause) => cause match {
            case End | Kill =>
              chunks = Nil
            case Cause.Error(e: Error) => throw e
            case Cause.Error(e: Throwable) => throw new IOException(e)
          }
          case s: Step[F, ByteVector] => s.head match {
            case em: Emit[ByteVector] =>
              assert(false)
            case aw: Await[F, _, ByteVector] =>
              cur = Try(aw.rcv(EarlyCause.fromTaskResult(runner(Catchable[F].attempt(aw.req)))).run) +: s.next
              close()
          }
        }
      }
    }

    @tailrec
    def step(): Unit = {
      index = 0
      val haltOrStep: HaltOrStep[F, ByteVector] = cur.step
      haltOrStep match {
        case h @ Halt(cause) => cause match {
          case End | Kill =>
            cur = h
          case Cause.Error(e: Error) =>
            cur = h
            throw e
          case Cause.Error(e: Throwable) =>
            cur = h
            throw new IOException(e)
        }
        case s: Step[F, ByteVector] => s.head match {
          case em: Emit[ByteVector] =>
            chunks = em.seq
            cur = s.next.continue
          case aw: Await[F, _, ByteVector] =>
            cur = Try(aw.rcv(EarlyCause.fromTaskResult(runner(Catchable[F].attempt(aw.req)))).run) +: s.next
            step() // push things onto the stack and then step further (tail recursively)
        }
      }
    }
  }

  def safe[F[_]: MonadAsync: Catchable](is: => InputStream): Process[F, ByteVector] =
    Process.constant(bufferSize).asInstanceOf[Process[F, Int]].through[F, ByteVector](chunkR[F](is))

  def chunkR[F[_]: MonadAsync: Catchable](is: => InputStream): Channel[F, Int, ByteVector] =
    unsafeChunkR(is).map(f => (n: Int) =>
      MonadAsync[F].monad.map(f(new Array[Byte](n)))(ByteVector.view))

  private def unsafeChunkR[F[_]: MonadAsync: Catchable](is: => InputStream): Channel[F, Array[Byte], Array[Byte]] = {
    implicit val monad = MonadAsync[F].monad
    resource(lazyReference(attempt(is)))(
      src => attempt(src.close())
    ) { src =>
        MonadAsync[F].now { (buf: Array[Byte]) =>
          val m = src.read(buf)
          if (m == buf.length) {
            buf.now
          } else if (m == -1) {
            Catchable[F].fail[Array[Byte]](Cause.Terminated(Cause.End))
          } else {
            buf.take(m).now
          }
        }
      }
  }

  def decodeMmap[F[_]: MonadAsync: Catchable, A](sc: StreamCodec[A], in: => FileChannel, chunkSizeInBytes: Int = 1024 * 1000 * 16): Process[F, A] = {
    implicit val monad = MonadAsync[F].monad

    def decode(bits: => BitVector): Process[F, A] = Process.suspend {
      @volatile var cur = bits // am pretty sure this doesn't need to be volatile, but just being safe
      sc.decoder.translate(new (Cursor ~> F) {
        def apply[X](c: Cursor[X]): F[X] = MonadAsync[F].suspend {
          c.run(cur).fold(
            msg => Catchable[F].fail(DecodingError(msg)),
            res => MonadAsync[F].now {
              cur = res.remainder; res.value
            }
          )
        }
      })
    }

    def decodeResource[R](acquire: => R)(
      read: R => BitVector,
      release: R => Unit
    ): Process[F, A] =
      decodeAsyncResource(attempt(acquire))(read, r => attempt(release(r)))

    def decodeAsyncResource[R](acquire: F[R])(
      read: R => BitVector,
      release: R => F[Unit]
    ): Process[F, A] =
      Process.eval(acquire).flatMap { r =>
        decode(read(r)) onComplete { Process.eval_(release(r)) }
      }

    decodeResource(in)(BitVector.fromMmap(_, chunkSizeInBytes), _.close)
  }

  private def lazyReference[F[_]: MonadAsync: Monad: Catchable, A](fa: F[A]): F[A] =
    Atomic.synchronized[F, A].getOrSet(fa)

  private def attempt[F[_]: MonadAsync: Monad: Catchable, A](a: => A): F[A] =
    MonadAsync[F].suspend(tryCatch(a))

  private object Attachment

  def asyncChunkW[F[_]: MonadAsync: Catchable](f: Path, append: Boolean = false): Sink[F, ByteVector] = {
    implicit val monad = MonadAsync[F].monad
    val pos = new AtomicLong(if (append) Files.size(f) else 0)
    resource(
      lazyReference(attempt {
        AsynchronousFileChannel.open(f, StandardOpenOption.CREATE, StandardOpenOption.WRITE, if (append) StandardOpenOption.APPEND else StandardOpenOption.TRUNCATE_EXISTING)
      })
    )(
        src => attempt { src.close() }
      )(src =>
          MonadAsync[F].now { (bv: ByteVector) =>
            val buff = bv.toByteBuffer
            MonadAsync[F].async[Throwable \/ Unit] { cb: (Throwable \/ Unit => Unit) =>
              def go(): Unit = {
                src.write(buff, pos.get, Attachment, new CompletionHandler[Integer, Attachment.type] {
                  override def completed(result: Integer, attachment: Attachment.type): Unit = {
                    pos.addAndGet(result.toLong)
                    if (buff.hasRemaining) {
                      go()
                    } else {
                      cb(\/-(()))
                    }
                  }
                  override def failed(exc: Throwable, attachment: Attachment.type): Unit =
                    cb(-\/(exc))
                })
              }
              go()
            }.unattempt
          })
  }

  def fileChunkW[F[_]: MonadAsync: Catchable](f: Path, append: Boolean = false): Sink[F, ByteVector] = {
    implicit val monad = MonadAsync[F].monad
    resource(
      lazyReference(attempt {
        Files.newByteChannel(
          f,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE,
          if (append) StandardOpenOption.APPEND else StandardOpenOption.TRUNCATE_EXISTING
        )
      })
    )(
        src => attempt { src.close() }
      )(
          src => MonadAsync[F].now((bv: ByteVector) => attempt {
            val buff = bv.toByteBuffer
            while (buff.hasRemaining) {
              src.write(buff)
            }
          })
        )
  }

}
