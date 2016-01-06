package monadasync

sealed trait Timeout
object Timeout {
  private val instance = new Timeout {}
  def apply(): Timeout = instance
}
