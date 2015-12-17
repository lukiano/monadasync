package io.atlassian.monadasync
package stream
package file

import java.io.{ ByteArrayOutputStream, DataInputStream, IOException, InputStream, OutputStream }

import org.scalacheck.Prop.{ forAll, secure }
import scodec.bits.ByteVector

import scalaz._
import scalaz.stream.Process
import MonadSuspend.syntax._

abstract class InputStreamSpec extends ImmutableSpec with ByteVectorArbitraries with ByteOps {

  type F[A]

  implicit def Runner: F ~> Id.Id

  implicit def MS: MonadSuspend[F]
  implicit def C: Catchable[F]

  def is = s2"""
    This is a specification to check ToInputStream

    toInputStream should
      handles arbitrary emitAll                                          $handlesArbitraryEmitAll
      handles appended emits                                             $handlesAppendedEmits
      handles await                                                      $handlesAwait
      handles appended awaits                                            $handlesAppendedAwaits
      handles one append within an await                                 $handlesOneAppendWithinAnAwait
      handles appends within awaits                                      $handlesAppendsWithinAwaits
      invokes finalizers when terminated early                           $invokesFinalizersWhenTerminatedEarly
      safely read byte 255 as an Int                                     $safelyReadByte255AsAnInt
      provide a valid input stream                                       $providesValidInputStream
      provide an input stream with a close that works                    $inputStreamCloseWorks
      throw the exception that cause the stream to terminate prematurely $throwsExceptionOnPrematureTermination
      read from chunks                                                   $readFromChunks
  """

  def handlesArbitraryEmitAll = forAll { bytes: ShortList[ByteVector] =>
    val length = bytes.list map { _.length } sum
    val p = Process.emitAll(bytes.list)

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat(bytes)
  }

  def handlesAppendedEmits = forAll { bytes: ShortList[ByteVector] =>
    val length = bytes.list map { _.length } sum
    val p = (bytes.list map Process.emit) reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat(bytes)
  }

  def handlesAwait = forAll { chunk: ByteVector =>
    val length = chunk.length

    val p = Process.await(().now[F]) { _ =>
      Process.emit(chunk)
    }

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === chunk
  }

  def handlesAppendedAwaits = forAll { bytes: ShortList[ByteVector] =>
    val length = bytes.list map { _.length } sum

    val p = bytes.list map { data =>
      Process.await(().now[F]) { _ =>
        Process.emit(data)
      }
    } reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat(bytes)
  }

  def handlesOneAppendWithinAnAwait = secure {
    val bytes: ShortList[ShortList[ByteVector]] = ShortList(ShortList.empty, ShortList(ByteVector(127)))
    val length = bytes.list map { _.list map { _.length } sum } sum

    val p: Process[F, ByteVector] = bytes.list map { data =>
      Process.await(().now[F]) { _ =>
        data.list map Process.emit reduceOption { _ ++ _ } getOrElse Process.empty
      }
    } reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat2(bytes)
  }

  def handlesAppendsWithinAwaits = forAll { bytes: ShortList[ShortList[ByteVector]] =>
    val length = bytes.list map { _.list map { _.length } sum } sum

    val p = bytes.list map { data =>
      Process.await(().now[F]) { _ =>
        data.list map Process.emit reduceOption { _ ++ _ } getOrElse Process.empty
      }
    } reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()
    ByteVector(buffer) === concat2(bytes)
  }

  def invokesFinalizersWhenTerminatedEarly = secure {
    import scalaz.stream.Process._

    var flag = false
    val setter = MonadSuspend[F].delay { flag = true }

    val p = (emit(ByteVector(42)) ++ emit(ByteVector(24))) onComplete (Process eval_ setter)

    val is = toInputStream(p)

    val read = is.read()
    is.close()

    flag :| "finalizer flag" &&
      (read == 42) :| "read value"
  }

  def safelyReadByte255AsAnInt = secure {
    val p = Process emit ByteVector(-1)
    val is = toInputStream(p)

    is.read() === 255
  }

  def providesValidInputStream =
    forAll { (content: Array[Byte]) =>
      val is = streamFor(content)

      val streamInputStream = toInputStream {
        safe[F](is)
      }
      (streamInputStream must matchContent(streamFor(content))) and
        testStreamsClosed(is, streamInputStream) === true
    }

  private def streamFor(content: Array[Byte]): InputStream =
    new ClosingByteArrayInputStream(content)

  def inputStreamCloseWorks =
    forAll { (content: Array[Byte]) =>
      (content.length > 2) ==> {
        val is = streamFor(content)
        testStreamsClosed(
          is,
          toInputStream {
            safe[F](is)
          } unsafeTap { inputStream =>
            inputStream.read() // must read at least one byte to call acquire on unsafeChunkR
            inputStream.close()
          }
        )
      }
    }

  implicit class UnSafeTap[A](a: A) {
    def unsafeTap[B](f: A => B): A = {
      f(a)
      a
    }
  }

  private def testStreamsClosed(streams: InputStream*) =
    streams.forall { s =>
      s.available === 0 and s.read === -1
    }

  def throwsExceptionOnPrematureTermination = {
    var currentCount = 0
    val is = toInputStream {
      Process.repeat(Process.suspend(Process.eval {
        MonadSuspend[F].delay {
          val random = scala.util.Random
          currentCount += 1
          if (random.nextDouble() < 0.2 || currentCount >= 100)
            throw new Exception("FOO")
          else
            ByteVector(random.nextInt(100))
        }
      }))
    }

    Stream.continually {
      is.read()
    }.takeWhile {
      -1 !=
    }.map {
      _.toByte
    }.toArray must throwA[IOException]
  }

  def readFromChunks = {
    def source: Int => F[ByteVector] = _ =>
      MonadSuspend[F].delay(ByteVector.fill(1000)(0))

    val trans: Iterator[F[ByteVector]] = (1 to 10).map(source).iterator

    def readChunks: Process[F, ByteVector] = {
      def loop: Process[F, ByteVector] =
        Process.await[F, ByteVector, ByteVector](trans.next()) {
          case byteArray =>
            Process.emit {
              byteArray
            }.append[F, ByteVector] {
              if (!trans.hasNext) Process.halt
              else loop
            }
        }
      loop
    }

    def copyData(in: InputStream, out: OutputStream): Unit = {
      val buffer: Array[Byte] = new Array(18192)
      @scala.annotation.tailrec
      def go(): Unit = {
        val bytesRead = in.read(buffer)
        if (bytesRead > 0) {
          out.write(buffer, 0, bytesRead)
          go()
        }
      }
      try {
        go()
      } finally {
        out.close()
        in.close()
      }
    }

    val is = toInputStream {
      Process.await[F, Unit, ByteVector](().now[F]) { _ =>
        readChunks
      }
    }

    val baos = new ByteArrayOutputStream()
    copyData(is, baos)
    baos.toByteArray.length === 10000
  }
}

