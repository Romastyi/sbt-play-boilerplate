package play.boilerplate.api.server.dsl

import play.api.mvc.PathBindable
import play.api.routing.sird.PathBindableExtractor

object SirdOps {

  /**
    * An List[T] extractors.
    */
  def listOf[T](separator: Char)(implicit pb: PathBindable[T]): PathBindableExtractor[List[T]] = {
    implicit val pl: PathBindable[List[T]] = new PathBindable[List[T]] {
      override def bind(key: String, values: String): Either[String, List[T]] = {
        Right(
          for {
            rawValue <- values.trim.split(separator).toList
            bound <- pb.bind("anon", rawValue).right.toOption
          } yield bound
        )
      }
      override def unbind(key: String, values: List[T]): String = {
        (for (value <- values) yield {
          pb.unbind("anon", value).replaceAll("anon=", "")
        }).mkString(separator.toString)
      }
    }
    new PathBindableExtractor[List[T]]
  }

}
