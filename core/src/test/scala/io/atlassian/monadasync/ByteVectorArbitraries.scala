package io.atlassian.monadasync

import java.nio.ByteBuffer

import org.scalacheck.{ Arbitrary, Gen, Shrink }
import scodec.bits.ByteVector

trait ByteVectorArbitraries {

  private def bytesGen: Gen[Array[Byte]] = Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)

  private def standardByteVectors(maxSize: Int): Gen[ByteVector] = for {
    bytes <- bytesGen
  } yield ByteVector(bytes)

  val sliceByteVectors: Gen[ByteVector] = for {
    bytes <- bytesGen
    toDrop <- Gen.choose(0, bytes.length)
  } yield ByteVector.view(bytes).drop(toDrop)

  private def genSplit(g: Gen[ByteVector]) = for {
    b <- g
    n <- Gen.choose(0, b.size + 1)
  } yield {
    b.take(n) ++ b.drop(n)
  }

  private def genByteBufferVectors(maxSize: Int): Gen[ByteVector] = for {
    bytes <- bytesGen
  } yield ByteVector.view(ByteBuffer.wrap(bytes))

  private def genConcat(g: Gen[ByteVector]) =
    g.map { b => b.toIndexedSeq.foldLeft(ByteVector.empty)(_ :+ _) }

  private def byteVectors: Gen[ByteVector] = Gen.oneOf(
    standardByteVectors(100),
    genConcat(standardByteVectors(100)),
    sliceByteVectors,
    genSplit(sliceByteVectors),
    genSplit(genConcat(standardByteVectors(500))),
    genByteBufferVectors(100)
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

}