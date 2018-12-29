package play.boilerplate.api.server.dsl

import play.api.mvc.{PathBindable, QueryStringBindable}
import play.api.routing.sird.{PathBindableExtractor, QueryString, QueryStringParameterExtractor}

object SirdOps {

  /**
    * Path parameter extractor
    */
  def pathOf[T](implicit pb: PathBindable[T]): PathBindableExtractor[T] = new PathBindableExtractor[T]

  /**
    * Query parameter extractor
    */
  def queryOf[T](paramName: String)(implicit qb: QueryStringBindable[T]): QueryStringParameterExtractor[T] = new QueryStringParameterExtractor[T] {
    override def unapply(qs: QueryString): Option[T] = qb.bind(paramName, qs).collect {
      case Right(value) => value
    }
  }

}
