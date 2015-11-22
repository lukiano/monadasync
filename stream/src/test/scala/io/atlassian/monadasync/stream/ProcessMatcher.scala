package io.atlassian.monadasync.stream

import java.io.ByteArrayOutputStream

import ProcessMatcher.ByteProcess
import org.apache.commons.io.IOUtils
import scodec.bits.ByteVector

import scalaz.concurrent.Task
import scalaz.stream.{Process, io}
import org.specs2.matcher.{Expectable, MatchResult, Matcher}

object ProcessMatcher {
  type ByteProcess = Process[Task, ByteVector]
  val chunkSize = 1024
}

class ProcessMatcher(expected: Array[Byte]) extends Matcher[ByteProcess] {

  override def apply[S <: ByteProcess](s: Expectable[S]): MatchResult[S] = {
    val is = io.toInputStream(s.value)
    val baos = new ByteArrayOutputStream
    IOUtils.copy(is, baos)
    val actual: Array[Byte] = baos.toByteArray
    IOUtils.closeQuietly(is)

    if (actual.length != expected.length) {
      result[S](false, "", s"Different content size, expected ${expected.length} got ${actual.length}", s)
    } else {
      val (check, message) = ArrayMatcher.checkContent(expected, actual)
      result[S](check, message, message, s)
    }
  }
}