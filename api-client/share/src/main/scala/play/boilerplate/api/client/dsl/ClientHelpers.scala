package play.boilerplate.api.client.dsl

import com.fasterxml.jackson.core.JsonParseException
import play.api.libs.json.{JsResultException, JsValue, Reads}

import scala.language.implicitConversions
import scala.util.Try
import scala.util.control.NoStackTrace

trait ClientHelpers {

  import Compat._

  case class JsonParsingError(cause: Throwable, body: String, code: Int, contentType: String) extends Throwable with NoStackTrace {
    override def getMessage: String = {
      "JSON parsing error: " + cause.getMessage + "\nOriginal body: " + body
    }
  }

  case class JsonValidationError(cause: Throwable, body: JsValue, code: Int, contentType: String) extends Throwable with NoStackTrace {
    override def getMessage: String = {
      "JSON validation error: " + cause.getMessage + "\nOriginal body: " + body
    }
  }

  case class UnexpectedResponseError(cause: Throwable, body: String, code: Int, contentType: String) extends Throwable with NoStackTrace {
    override def getMessage: String = {
      "Unexpected response: " + code + " " + body
    }
  }

  protected def parseResponseAsJson[A](response: WSResponse)(implicit rs: Reads[A]): A = {
    Try(response.json.as[A]).recover({
      case cause: JsonParseException => throw JsonParsingError(cause, response.body, response.status, getContentType(response))
      case cause: JsResultException => throw JsonValidationError(cause, response.json, response.status, getContentType(response))
      case cause => throw UnexpectedResponseError(cause, response.body, response.status, getContentType(response))
    }).get
  }

  protected def getContentType(response: WSResponse): String = response.header("Content-Type").getOrElse("text/plain")

  protected def _render_path_param[A : PathParameter](key: String, value: A): String = {
    PathParameter.render(value)
  }

  sealed trait QueryValueWrapper
  private case class QueryValueWrapperImpl(render: String => String) extends QueryValueWrapper

  implicit def toQueryValueWrapper[T : QueryParameter](value: T): QueryValueWrapper = {
    QueryValueWrapperImpl(QueryParameter.render(_, value))
  }

  protected def _render_url_params(pairs: (String, QueryValueWrapper)*): String = {
    val parts = pairs.collect({
      case (k, QueryValueWrapperImpl(render)) => render(k)
    }).filter(_.nonEmpty)
    if (parts.nonEmpty) parts.mkString("?", "&", "")
    else ""
  }

  protected def _render_header_params(pairs: (String, Option[Any])*): Seq[(String, String)] = {
    pairs.collect({
      case (k, Some(v)) => k -> v.toString
    })
  }

  protected def printFormUrlEncoded(data: Map[String, Seq[String]]): String = {
    (for {
      (name, values) <- data
      value <- values
    } yield name + "=" + value).mkString("\n")
  }

}
