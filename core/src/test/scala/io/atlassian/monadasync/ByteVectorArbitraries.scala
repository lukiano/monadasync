package io.atlassian.monadasync

import java.nio.ByteBuffer

import org.scalacheck.{ Arbitrary, Gen, Shrink }
import scodec.bits.ByteVector
import scalaz.syntax.id._

import scala.util.Random

trait ByteVectorArbitraries {

  private val random = new Random

  private def bytesGen(maxSize: Int): Gen[Array[Byte]] =
    Gen.chooseNum(1, maxSize, 1, 4096, 8192, 16384, 32768) map { new Array[Byte](_) <| random.nextBytes }

  private def standardByteVectors(maxSize: Int): Gen[ByteVector] = for {
    bytes <- bytesGen(maxSize)
  } yield ByteVector(bytes)

  def sliceByteVectors(maxSize: Int): Gen[ByteVector] = for {
    bytes <- bytesGen(maxSize)
    toDrop <- Gen.choose(0, bytes.length)
  } yield ByteVector.view(bytes).drop(toDrop)

  private def genSplit(g: Gen[ByteVector]) = for {
    b <- g
    n <- Gen.choose(0, b.size + 1)
  } yield {
    b.take(n) ++ b.drop(n)
  }

  private def genByteBufferVectors(maxSize: Int): Gen[ByteVector] = for {
    bytes <- bytesGen(maxSize)
  } yield ByteVector.view(ByteBuffer.wrap(bytes))

  private def genConcat(g: Gen[ByteVector]) =
    g map { b => b.grouped(256).foldLeft(ByteVector.empty)(_ ++ _) }

  private val maxSize: Int = 100000

  private def byteVectors: Gen[ByteVector] = Gen.oneOf(
    standardByteVectors(maxSize),
    genConcat(standardByteVectors(maxSize)),
    sliceByteVectors(5 * maxSize),
    genSplit(sliceByteVectors(5 * maxSize)),
    genSplit(genConcat(standardByteVectors(5 * maxSize))),
    genByteBufferVectors(maxSize)
  )

  implicit val arbitraryByteVectors: Arbitrary[ByteVector] = Arbitrary(byteVectors)

  implicit val shrinkByteVector: Shrink[ByteVector] =
    Shrink[ByteVector] { b =>
      if (b.nonEmpty) {
        Stream.iterate(b.take(b.size / 2))(b2 => b2.take(b2.size / 2)).takeWhile(_.nonEmpty) ++ Stream(ByteVector.empty)
      } else {
        Stream.empty
      }
    }

  case class ShortList[A](list: List[A])

  object ShortList {
    def apply[A](a: A*): ShortList[A] = ShortList(a.toList)
    def empty[A]: ShortList[A] = ShortList(Nil)
  }

  implicit def arbitraryShortList[A](implicit AA: Arbitrary[A]): Arbitrary[ShortList[A]] =
    Arbitrary {
      for {
        n <- Gen.oneOf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
        list <- Gen.listOfN(n, AA.arbitrary)
      } yield ShortList(list)
    }

  def concat(shortList: ShortList[ByteVector]): ByteVector =
    ByteVector.concat(shortList.list)

  def concat2(shortList: ShortList[ShortList[ByteVector]]): ByteVector =
    shortList.list.foldLeft(ByteVector.empty)(_ ++ concat(_))
}