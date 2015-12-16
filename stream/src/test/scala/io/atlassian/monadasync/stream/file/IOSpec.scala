package io.atlassian.monadasync
package stream
package file

import java.io.ByteArrayInputStream
import java.nio.file.Files

import org.junit.runner.RunWith
import org.scalacheck.Prop

import scalaz.concurrent.Task

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class IOSpec extends ImmutableSpec with ByteVectorArbitraries with ByteOps {

  import MonadAsync._

  def is = s2"""
      IO should
        synchronously write all data to a file  $fileChunkW
        asynchronously write all data to a file $asyncChunkW
    """

  def fileChunkW = Prop.forAll { bytes: Array[Byte] =>
    val toWrite = Files.createTempFile(null, null)
    file.safe[Task](new ByteArrayInputStream(bytes)).through(file.fileChunkW[Task](toWrite)).run.run
    val writtenBytes = Files.readAllBytes(toWrite)
    Files.delete(toWrite)
    writtenBytes must matchByteContent(bytes)
  }

  def asyncChunkW = Prop.forAll { bytes: Array[Byte] =>
    val toWrite = Files.createTempFile(null, null)
    file.safe[Task](new ByteArrayInputStream(bytes)).through(file.asyncChunkW[Task](toWrite)).run.run
    val writtenBytes = Files.readAllBytes(toWrite)
    Files.delete(toWrite)
    writtenBytes must matchByteContent(bytes)
  }

}
