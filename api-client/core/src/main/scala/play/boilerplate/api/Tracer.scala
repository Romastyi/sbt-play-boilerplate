package play.boilerplate.api

trait Tracer {
  def applyMsg(msg: String): String
  def traceId: String
}

object Tracer {

  def apply(_traceId: String): Tracer = new Tracer {
    override def applyMsg(msg: String): String = "[TraceID " + traceId + "] " + msg
    override val traceId: String = _traceId
  }

  def empty: Tracer = apply("")

  def random(length: Int): Tracer = apply(scala.util.Random.alphanumeric.take(length).mkString(""))

  def randomUUID: Tracer = apply(java.util.UUID.randomUUID().toString)

}
