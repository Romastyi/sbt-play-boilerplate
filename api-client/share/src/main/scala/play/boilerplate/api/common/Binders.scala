package play.boilerplate.api.common

import play.api.mvc.QueryStringBindable

import language.experimental.macros
import scala.reflect.ClassTag

// TODO Must be a part of server api (not client api!)
object Binders {

  /**
    * Generates QueryStringBindable, for which this parameter must be present in the query-parameters
    */
  def queryStringStrict[A]: QueryStringBindable[A] = macro BindersMacroImpl.queryStringStrictImpl[A]

  /**
    * Generates QueryStringBindable, for which this parameter is not necessarily present in the query-parameters
    */
  def queryString[A]: QueryStringBindable[A] = macro BindersMacroImpl.queryStringImpl[A]

  /**
    * QueryString binder for Seq as collection with separator
    */
  def querySeq[A : ClassTag](separator: Char)(implicit binder: QueryStringBindable[A]): QueryStringBindable[Seq[A]] = {

    def bindList(values: String): Seq[A] = {
      for {
        rawValue <- values.trim.split(separator).toList
        bound <- binder.bind("anon", Map("anon" -> Seq(rawValue)))
        value <- bound.right.toOption
      } yield value
    }

    def serializeList(values: Seq[A]): String = {
      (for (value <- values) yield {
        binder.unbind("anon", value).replaceAll("anon=", "")
      }).mkString(separator.toString)
    }

    new QueryStringBindable.Parsing[Seq[A]](
      bindList,
      serializeList,
      (key: String, _: Exception) => s"Cannot parse parameter $key as collection of ${implicitly[ClassTag[A]].runtimeClass.getCanonicalName} with separator '$separator'."
    )

  }

  def queryList[A : ClassTag : QueryStringBindable](separator: Char): QueryStringBindable[List[A]] =
    querySeq(separator).transform(_.toList, _.toSeq)

}