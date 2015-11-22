package io.atlassian.monadasync
package stream
package file

import java.io.{ ByteArrayOutputStream, DataInputStream, IOException, InputStream, OutputStream }

import org.junit.runner.RunWith
import scodec.bits.ByteVector

import scala.collection.GenTraversableOnce
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import org.scalacheck.Prop.{ forAll, secure }

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ToInputStreamSpec extends ImmutableSpec with ByteVectorArbitraries with ByteOps {

  import Task._

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

  implicit val Runner: Task ~> Id.Id = new (Task ~> Id.Id) {
    def apply[A](fr: Task[A]): A =
      fr.run
  }

  def handlesArbitraryEmitAll = forAll { bytes: List[ByteVector] =>
    val length = bytes map { _.length } sum
    val p = Process.emitAll(bytes)

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat(bytes)
  }

  def handlesAppendedEmits = forAll { bytes: List[ByteVector] =>
    val length = bytes map { _.length } sum
    val p = (bytes map Process.emit) reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat(bytes)
  }

  def handlesAwait = forAll { chunk: ByteVector =>
    val length = chunk.length

    val p = Process.await(Task now (())) { _ =>
      Process.emit(chunk)
    }

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === chunk
  }

  def handlesAppendedAwaits = forAll { bytes: List[ByteVector] =>
    val length = bytes map { _.length } sum

    val p = bytes map { data =>
      Process.await(Task now (())) { _ =>
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
    val bytes: List[List[ByteVector]] = List(List(), List(ByteVector(127)))
    val length = bytes map { _ map { _.length } sum } sum

    val p: Process[Task, ByteVector] = bytes map { data =>
      Process.await(Task now (())) { _ =>
        data map Process.emit reduceOption { _ ++ _ } getOrElse Process.empty
      }
    } reduceOption { _ ++ _ } getOrElse Process.empty

    val dis = new DataInputStream(toInputStream(p))
    val buffer = new Array[Byte](length)
    dis.readFully(buffer)
    dis.close()

    ByteVector(buffer) === concat2(bytes)
  }

  def handlesAppendsWithinAwaits = forAll { bytes: List[List[ByteVector]] =>
    val length = bytes map { _ map { _.length } sum } sum

    val p = bytes map { data =>
      Process.await(Task now (())) { _ =>
        data map Process.emit reduceOption { _ ++ _ } getOrElse Process.empty
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
    val setter = Task delay { flag = true }

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
        file.safe[Task](is)
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
            file.safe[Task](is)
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
      Process.repeatEval {
        Task.delay {
          val random = scala.util.Random
          currentCount += 1
          if (random.nextDouble() < 0.2 || currentCount >= 100)
            throw new Exception("FOO")
          else
            ByteVector(random.nextInt(100))
        }
      }
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
    def source: Int => Task[ByteVector] = _ =>
      Task.delay(ByteVector.fill(1000)(0))

    val trans: Iterator[Task[ByteVector]] = (1 to 10).map(source).iterator

    def readChunks: Process[Task, ByteVector] = {
      def loop: Process[Task, ByteVector] =
        Process.await[Task, ByteVector, ByteVector](trans.next()) {
          case byteArray =>
            Process.emit {
              byteArray
            }.append[Task, ByteVector] {
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
      Process.await[Task, Any, ByteVector](Task.now(())) { _ =>
        readChunks
      }
    }

    val baos = new ByteArrayOutputStream()
    copyData(is, baos)
    baos.toByteArray.length === 10000
  }

  private def concat(array: GenTraversableOnce[ByteVector]): ByteVector =
    ByteVector.concat(array)

  private def concat2(array: GenTraversableOnce[GenTraversableOnce[ByteVector]]): ByteVector =
    array.foldLeft(ByteVector.empty)(_ ++ concat(_))
}

