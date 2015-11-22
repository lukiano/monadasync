package io.atlassian.monadasync
package stream
package file

import java.io.ByteArrayInputStream

class ClosingByteArrayInputStream(buf: Array[Byte], offset: Int, length: Int)
    extends ByteArrayInputStream(buf, offset, length) {

  private var closed: Boolean = false

  def this(buf: Array[Byte]) = this(buf, 0, buf.length)

  override def read: Int = eofOr { super.read }

  override def read(b: Array[Byte]): Int = eofOr { super.read(b) }

  override def read(b: Array[Byte], off: Int, len: Int): Int = eofOr { super.read(b, off, len) }

  override def skip(n: Long): Long = if (closed) 0L else super.skip(n)

  override def available: Int = if (closed) 0 else super.available

  override def close(): Unit = {
    closed = true
    super.close()
  }

  private def eofOr(f: => Int): Int = if (closed) -1 else f

}
