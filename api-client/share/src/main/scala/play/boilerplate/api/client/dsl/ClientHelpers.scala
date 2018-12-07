package play.boilerplate.api.client.dsl

import play.api.libs.json._
import play.api.mvc._

import scala.language.implicitConversions

trait ClientHelpers {

  case class JsonParsingError(cause: java.lang.Throwable, body: String, code: Int, contentType: String) extends java.lang.Throwable with scala.util.control.NoStackTrace {
    override def getMessage: String = {
      "JSON parsing error: " + cause.getMessage + "\nOriginal body: " + body
    }
  }

  case class JsonValidationError(cause: java.lang.Throwable, body: JsValue, code: Int, contentType: String) extends java.lang.Throwable with scala.util.control.NoStackTrace {
    override def getMessage: String = {
      "JSON validation error: " + cause.getMessage + "\nOriginal body: " + body
    }
  }

  case class UnexpectedResponseError(cause: java.lang.Throwable, body: String, code: Int, contentType: String) extends java.lang.Throwable with scala.util.control.NoStackTrace {
    override def getMessage: String = {
      "Unexpected response: " + code + " " + body
    }
  }

  protected def _render_path_param[A](key: String, value: A)(implicit pb: PathBindable[A]): String = {
    pb.unbind(key, value)
  }

  sealed trait QueryValueWrapper
  private case class QueryValueWrapperImpl(unbind: String => String) extends QueryValueWrapper

  implicit def toQueryValueWrapper[T](value: T)(implicit qb: QueryStringBindable[T]): QueryValueWrapper = {
    QueryValueWrapperImpl(qb.unbind(_, value))
  }

  protected def _render_url_params(pairs: (String, QueryValueWrapper)*): String = {
    val parts = pairs.collect({
      case (k, QueryValueWrapperImpl(unbind)) => unbind(k)
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
