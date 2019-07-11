package play.boilerplate.api

trait Tracer {
  def applyMsg(msg: String): String
  def traceId: String
  def transform(f: String => String): Tracer = Tracer.transform(this)(f)
}

object Tracer {

  val mdcTraceId = "X-TraceId"

  def apply(_traceId: String): Tracer = new Tracer {
    override def applyMsg(msg: String): String = "[TraceID " + traceId + "] " + msg
    override val traceId: String = _traceId
  }

  def random(length: Int): Tracer = apply(scala.util.Random.alphanumeric.take(length).mkString(""))

  def randomUUID: Tracer = apply(java.util.UUID.randomUUID().toString)

  def transform(t: Tracer)(f: String => String): Tracer = new Tracer {
    override def applyMsg(msg: String): String = t.applyMsg(f(msg))
    override def traceId: String = t.traceId
  }

}
