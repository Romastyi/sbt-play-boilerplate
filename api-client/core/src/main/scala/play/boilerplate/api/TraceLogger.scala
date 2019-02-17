package play.boilerplate.api

trait TraceLogger {

  protected def errorInternal(msg: => String, error: => Throwable): Unit
  def error(msg: => String, error: => Throwable)(implicit tracer: Tracer): Unit = errorInternal(tracer.applyMsg(msg), error)

  protected def traceInternal(msg: => String): Unit
  def trace(msg: => String)(implicit tracer: Tracer): Unit = traceInternal(tracer.applyMsg(msg))

}
