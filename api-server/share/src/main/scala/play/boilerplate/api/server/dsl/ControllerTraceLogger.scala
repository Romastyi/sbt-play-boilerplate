package play.boilerplate.api.server.dsl

import play.api.mvc._
import play.boilerplate.api.{TraceLogger, Tracer}

trait ControllerTraceLogger extends TraceLogger {

  def putMDC(implicit tracer: Tracer): Unit = org.slf4j.MDC.put(Tracer.mdcTraceId, tracer.traceId)

  private def operationTracer(operationId: String)(implicit tracer: Tracer): Tracer =
    tracer.transform(msg => "[operationId: " + operationId + "] " + msg)

  protected def printRequest[A](request: Request[A], body: String): String

  def logRequest[A](operationId: String, request: Request[A])(implicit tracer: Tracer, pr: PrintableContent[A]): Unit = {
    trace(printRequest(request, pr.contentAsString(request.body)))(operationTracer(operationId))
  }

  protected def printResponse(response: Result, body: String, contentType: String): String

  def logResponse[C](operationId: String, response: Result, body: C, contentType: String)(implicit tracer: Tracer, pr: PrintableContent[C]): Unit = {
    trace(printResponse(response, pr.contentAsString(body), contentType))(operationTracer(operationId))
  }

  def logError(operationId: String, msg: => String, cause: => Throwable)(implicit tracer: Tracer): Unit = {
    error(msg, cause)(operationTracer(operationId))
  }

}

object ControllerTraceLogger {

  object NoLogger extends ControllerTraceLogger {
    override protected def printRequest[A](request: Request[A], body: String): String = ""
    override protected def printResponse(response: Result, body: String, contentType: String): String = ""
    // TraceLogger
    override protected def errorInternal(msg: => String, error: => Throwable): Unit = ()
    override protected def traceInternal(msg: => String): Unit = ()
  }

  abstract class Default extends ControllerTraceLogger {

    private def printHeaders(headers: Seq[(String, String)]): String = headers.map { case (name, value) =>
      name + ": " + value
    }.mkString("\n")

    override protected def printRequest[A](request: Request[A], body: String): String = {
      s"""REQUEST:
         |${request.method} ${request.uri}
         |${printHeaders(request.headers.headers)}
         |
         |$body
      """.stripMargin
    }

    override protected def printResponse(response: Result, body: String, contentType: String): String = {
      s"""RESPONSE:
         |${response.header.status}
         |${printHeaders(response.header.headers.toIndexedSeq)}
         |Content-Type: $contentType
         |Content-Length: ${body.length}
         |
         |$body
      """.stripMargin
    }

  }

}