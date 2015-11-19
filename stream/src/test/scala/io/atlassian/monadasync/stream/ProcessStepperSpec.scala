package io.atlassian.monadasync
package stream

import java.io.ByteArrayInputStream

import scodec.bits.ByteVector

import scalaz.concurrent.Task
import scalaz.stream.io.chunkR
import scalaz.stream.{Process, Process1, process1}

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ProcessStepperSpec extends BlobstoreSpec {

  def is = s2"""
      ProcessStepper should
        read all data from an InputStream                        $stepFromInputStream
        read all data from an InputStream piped through Process1 $stepFromInputStreamPipedProcess1
    """

  def stepFromInputStream = Prop.forAll { bc: BlobContent =>
    val bytes = bc.unwrap
    val process = Process.constant(4096).toSource.through(chunkR(new ByteArrayInputStream(bytes)))
    val reader = new ProcessStepper[Task, ByteVector](process)
    def loop(left: ByteVector): Task[ByteVector] =
      reader.read flatMap {
        case Some(right) => loop(left |+| right)
        case None        => Task.now(left)
      }

    loop(ByteVector.empty).run.toArray must matchByteContent(bytes)
  }

  def stepFromInputStreamPipedProcess1 = Prop.forAll { bc: BlobContent =>
    val bytes = bc.unwrap
    def process = Process.constant(4096).toSource.through(chunkR(new ByteArrayInputStream(bytes)))
    val append = Array(255.toByte, 254.toByte)

    def loopProcess: Process1[ByteVector, ByteVector] =
      process1.drainLeading[ByteVector, ByteVector] {
        Process.receive1[ByteVector, ByteVector] { bv =>
          Process.emit(bv) ++ loopProcess
        }
      }

    def transducer: Process1[ByteVector, ByteVector] = Process.suspend {
      loopProcess
    } onComplete {
      Process.emit(ByteVector(append))
    }

    val reader = new ProcessStepper[Task, ByteVector](process.pipe(transducer))
    def loop(left: ByteVector): Task[ByteVector] =
      reader.read flatMap {
        case Some(right) => loop(left ++ right)
        case None        => Task.now(left)
      }

    (loop(ByteVector.empty).run.toArray must matchByteVector(ByteVector(bytes) |+| ByteVector(append))) and
      (process.pipe(transducer) must containSameContentAs(ByteVector(bytes) |+| ByteVector(append)))
  }
}
