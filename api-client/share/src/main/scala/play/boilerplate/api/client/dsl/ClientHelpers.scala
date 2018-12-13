package play.boilerplate.api.client.dsl

import play.api.libs.json.JsValue

import scala.language.implicitConversions
import scala.util.control.NoStackTrace

trait ClientHelpers {

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

}
