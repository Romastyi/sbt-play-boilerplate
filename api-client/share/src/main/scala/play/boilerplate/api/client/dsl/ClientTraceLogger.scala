package play.boilerplate.api.client.dsl

import Compat._
import play.boilerplate.api.{TraceLogger, Tracer}

trait ClientTraceLogger extends TraceLogger {

  protected def operationTracer(operationId: String)(implicit tracer: Tracer): Tracer =
    tracer.transform(msg => "[operationId: " + operationId + "] " + msg)

  protected def printRequest(method: String, request: WSRequest, body: String): String

  def logRequest(operationId: String, method: String, request: WSRequest, body: => String)(implicit tracer: Tracer): Unit = {
    trace(printRequest(method, request, body))(operationTracer(operationId))
  }

  protected def printResponse(response: WSResponse): String

  def logResponse(operationId: String, response: WSResponse)(implicit tracer: Tracer): Unit = {
    trace(printResponse(response))(operationTracer(operationId))
  }

  def logError(operationId: String, msg: => String, cause: => Throwable)(implicit tracer: Tracer): Unit = {
    error(msg, cause)(operationTracer(operationId))
  }

}

object ClientTraceLogger {

  object NoLogger extends ClientTraceLogger {
    override protected def printRequest(method: String, request: WSRequest, body: String): String = ""
    override protected def printResponse(response: WSResponse): String = ""
    // TraceLogger
    override protected def errorInternal(msg: => String, error: => Throwable): Unit = ()
    override protected def traceInternal(msg: => String): Unit = ()
  }

  abstract class Default extends ClientTraceLogger {

    private def printHeaders(headers: Map[String, Seq[String]]): String = {
      (for {
        (name, values) <- headers
        value <- values
      } yield name + ": " + value).mkString("\n")
    }

    override protected def printRequest(method: String, request: WSRequest, body: String): String = {
      s"""REQUEST:
         |$method ${request.url}
         |${printHeaders(request.headers)}
         |
         |$body
      """.stripMargin
    }

    override protected def printResponse(response: WSResponse): String = {
      s"""RESPONSE:
         |${response.status} ${response.statusText}
         |${printHeaders(response.headers)}
         |
         |${response.body}
      """.stripMargin
    }

  }

}
