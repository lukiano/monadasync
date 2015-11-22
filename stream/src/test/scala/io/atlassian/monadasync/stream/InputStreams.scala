package io.atlassian.monadasync.stream

import java.io.{BufferedInputStream, InputStream}

import org.specs2.matcher.{Expectable, Matcher}

import scala.annotation.tailrec
import scala.util.Random
import scalaz.syntax.id._
import scalaz.syntax.std.option._

object InputStreams {
  val bufferSize = 32768

  def random(seed: Int, size: Int): BufferedInputStream = random(seed.toLong, size.toLong)

  def random(seed: Long, size: Long): BufferedInputStream =
    new BufferedInputStream(new InputStream {
      private val rnd = new Random(seed)
      private var remaining: Long = size
      def read: Int =
        if (remaining <= 0) -1
        else {
          remaining -= 1
          rnd.nextInt(256)
        }
    }, bufferSize)

  implicit class InputStreamSyntax(input: InputStream) {
    final def size: Int = {
      val buffer = new Array[Byte](bufferSize)
      @tailrec
      def loop(i: Int): Int = {
        val b = input.read(buffer)
        if (b == -1) i else loop(i + b)
      }
      loop(0)
    }

    def toByteArray: Array[Byte] =
      Array.canBuildFrom[Byte].apply() |> { build =>
        @tailrec
        def loop(b: Int = input.read): Array[Byte] =
          if (b == -1) build.result
          else {
            build += b.toByte
            loop()
          }
        loop()
      }

    @tailrec
    final def drop(bytes: Long): InputStream =
      if (bytes == 0) input
      else {
        input.read
        drop(bytes - 1)
      }
  }

  class InputStreamMatcher(expected: InputStream) extends Matcher[InputStream] {
    private def checkContent(expected: InputStream, received: InputStream): Option[String] = {
      @annotation.tailrec
      def loop(i: Int): Option[String] = {
        val (expectedValue, resultValue) = (expected.read, received.read)
        if (expectedValue != resultValue) s"Content does not match at index $i: Expected $expectedValue but got $resultValue".some
        else if (resultValue == -1) None
        else loop(i + 1)
      }
      loop(0)
    }

    override def apply[S <: InputStream](s: Expectable[S]) = {
      val failed = checkContent(expected, s.value)
      result(failed.isEmpty, "Content and lengths match", failed getOrElse "", s)
    }
  }
}
