package play.boilerplate.api.server.dsl

import play.api.mvc.{PathBindable, QueryStringBindable}
import play.api.routing.sird.{PathBindableExtractor, QueryString, QueryStringParameterExtractor}

object SirdOps {

  /**
    * Path parameter extractor
    */
  def pathOf[T](implicit pb: PathBindable[T]): PathBindableExtractor[T] = new PathBindableExtractor[T]

  def pathOfRx(rx: String): PathBindableExtractor[String] = {
    val Pattern = rx.r
    new PathBindableExtractor[String]()(new PathBindable[String] {
      override def bind(key: String, value: String): Either[String, String] = value match {
        case Pattern(result) => Right(result)
        case _ => Left("Cannot parse parameter %s as String with pattern '%s' (actual: '%s')".format(key, rx, value))
      }
      override def unbind(key: String, value: String): String = value
    })
  }

  /**
    * Query parameter extractor
    */
  def queryOf[T](paramName: String)(implicit qb: QueryStringBindable[T]): QueryStringParameterExtractor[T] = new QueryStringParameterExtractor[T] {
    override def unapply(qs: QueryString): Option[T] = qb.bind(paramName, qs).collect {
      case Right(value) => value
    }
  }

  def queryOfRx(paramName: String, rx: String): QueryStringParameterExtractor[String] = {
    val Pattern = rx.r
    queryOf(paramName)(new QueryStringBindable[String] {
      def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, String]] = params.get(key).flatMap(_.headOption).map {
        case Pattern(result) => Right(result)
        case value => Left("Cannot parse parameter %s as String with pattern '%s' (actual: '%s')".format(key, rx, value))
      }
      override def unbind(key: String, value: String): String = key + "=" + value
    })
  }

}
