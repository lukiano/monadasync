package io.atlassian.monadasync
package stream

import java.io.InputStream

import scodec.bits.ByteVector

trait ByteOps {
  def matchByteContent(expected: Array[Byte]) =
    new ArrayMatcher(expected)

  def matchByteVector(expected: ByteVector) =
    matchByteContent(expected.toArray)

  def matchContent(expected: InputStream) =
    new InputStreams.InputStreamMatcher(expected)

  def containSameContent(expect: Array[Byte]) =
    new ProcessMatcher(expect)

  def containSameContentAs(expect: ByteVector) =
    containSameContent(expect.toArray)

}
