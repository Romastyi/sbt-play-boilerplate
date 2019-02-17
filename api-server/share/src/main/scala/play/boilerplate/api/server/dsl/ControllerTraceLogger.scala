package play.boilerplate.api.server.dsl

import play.api.mvc._
import play.boilerplate.api.{TraceLogger, Tracer}

trait ControllerTraceLogger extends TraceLogger {

  private def operationTracer(operationId: String)(implicit tracer: Tracer): Tracer =
    tracer.transform(msg => "[operationId: " + operationId + "] " + msg)

  protected def printRequest[A](request: Request[A]): String

  def logRequest[A](operationId: String, request: Request[A])(implicit tracer: Tracer): Unit = {
    trace(printRequest(request))(operationTracer(operationId))
  }

  protected def printResponse(response: Result, body: String, contentType: String): String

  def logResponse(operationId: String, response: Result, body: => String, contentType: => String)(implicit tracer: Tracer): Unit = {
    trace(printResponse(response, body, contentType))(operationTracer(operationId))
  }

  def logError(operationId: String, msg: => String, cause: => Throwable)(implicit tracer: Tracer): Unit = {
    error(msg, cause)(operationTracer(operationId))
  }

}

object ControllerTraceLogger {

  object NoLogger extends ControllerTraceLogger {
    override protected def printRequest[A](request: Request[A]): String = ""
    override protected def printResponse(response: Result, body: String, contentType: String): String = ""
    // TraceLogger
    override protected def errorInternal(msg: => String, error: => Throwable): Unit = ()
    override protected def traceInternal(msg: => String): Unit = ()
  }

  abstract class Default extends ControllerTraceLogger {

    private def printHeaders(headers: Seq[(String, String)]): String = headers.map { case (name, value) =>
      name + ": " + value
    }.mkString("\n")

    override protected def printRequest[A](request: Request[A]): String = {
      val bodyAsString = request.body match {
        case null | Unit | AnyContentAsEmpty =>
          ""
        case AnyContentAsText(txt) =>
          txt
        case AnyContentAsFormUrlEncoded(data) =>
          (for {
            (name, values) <- data
            value <- values
          } yield name + "=" + value).mkString("\n")
        case AnyContentAsRaw(raw) =>
          raw.toString()
        case AnyContentAsXml(xml) =>
          xml.map(scala.xml.Utility.trimProper).mkString
        case AnyContentAsJson(json) =>
          json.toString()
        case AnyContentAsMultipartFormData(data) =>
          data.toString
        case body =>
          body.toString
      }
      s"""REQUEST:
         |${request.method} ${request.uri}
         |${printHeaders(request.headers.headers)}
         |
         |$bodyAsString
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