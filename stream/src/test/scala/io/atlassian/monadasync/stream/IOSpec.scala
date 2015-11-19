package io.atlassian.monadasync
package stream

import java.io.ByteArrayInputStream
import java.nio.file.Files

import scalaz.concurrent.Task

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class IOSpec extends BlobstoreSpec with ByteVectorArbitraries {

  def is = s2"""
      IO should
        synchronously write all data to a file  $fileChunkW
        asynchronously write all data to a file $asyncChunkW
    """

  def fileChunkW = Prop.forAll { bc: BlobContent =>
    val bytes = bc.unwrap
    val toWrite = Files.createTempFile(null, null)
    io.safe[Task](new ByteArrayInputStream(bytes)).through(io.fileChunkW[Task](toWrite)).run.run
    val writtenBytes = Files.readAllBytes(toWrite)
    Files.delete(toWrite)
    writtenBytes must matchByteContent(bytes)
  }

  def asyncChunkW = Prop.forAll { bc: BlobContent =>
    val bytes = bc.unwrap
    val toWrite = Files.createTempFile(null, null)
    io.safe[Task](new ByteArrayInputStream(bytes)).through(io.asyncChunkW[Task](toWrite)).run.run
    val writtenBytes = Files.readAllBytes(toWrite)
    Files.delete(toWrite)
    writtenBytes must matchByteContent(bytes)
  }

}
