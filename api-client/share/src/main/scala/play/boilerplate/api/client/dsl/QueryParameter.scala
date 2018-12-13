package play.boilerplate.api.client.dsl

import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}

trait QueryParameter[A] { self =>

  def render(key: String, value: A): String

  def transform[B](f: B => A): QueryParameter[B] = new QueryParameter[B] {
    override def render(key: String, value: B): String = self.render(key, f(value))
  }

}

object QueryParameter {

  def apply[A : QueryParameter]: QueryParameter[A] = implicitly[QueryParameter[A]]

  def render[A : QueryParameter](key: String, value: A): String = apply[A].render(key, value)

  def concatAll(values: Seq[String]): String = values.filter(_.nonEmpty).mkString("&")

  private def renderSeq[T : QueryParameter](key: String, values: Seq[T]): String = {
    concatAll(for (value <- values) yield implicitly[QueryParameter[T]].render(key, value))
  }

  private def renderSeqWithSeparator[T : QueryParameter](key: String, values: Seq[T], separator: Char): String = {
    val value = (for (value <- values) yield render("anon", value).replaceAll("anon=", "")).mkString(separator.toString)
    stringQueryParameter.render(key, value)
  }

  implicit object stringQueryParameter extends QueryParameter[String] {
    override def render(key: String, value: String): String = key + "=" + value
  }

  implicit val charQueryParameter: QueryParameter[Char] =
    QueryParameter[String].transform(_.toString)

  implicit val intQueryParameter: QueryParameter[Int] =
    QueryParameter[String].transform(_.toString)

  implicit val longQueryParameter: QueryParameter[Long] =
    QueryParameter[String].transform(_.toString)

  implicit val doubleQueryParameter: QueryParameter[Double] =
    QueryParameter[String].transform(_.toString)

  implicit val floatQueryParameter: QueryParameter[Float] =
    QueryParameter[String].transform(_.toString)

  implicit val bigDecimalQueryParameter: QueryParameter[BigDecimal] =
    QueryParameter[String].transform(_.toString())

  implicit val booleanQueryParameter: QueryParameter[Boolean] =
    QueryParameter[String].transform(_.toString)

  implicit val uuidQueryParameter: QueryParameter[UUID] =
    QueryParameter[String].transform(_.toString)

  // Joda

  def jodaLocalDateParameter(pattern: String): QueryParameter[LocalDate] =
    QueryParameter[String].transform(_.toString(pattern))

  def jodaLocalTimeParameter(pattern: String): QueryParameter[LocalTime] =
    QueryParameter[String].transform(_.toString(pattern))

  def jodaDateTimeParameter(pattern: String): QueryParameter[DateTime] =
    QueryParameter[String].transform(_.toString(pattern))

  // Generic

  implicit def optionQueryParameter[A : QueryParameter]: QueryParameter[Option[A]] = {
    new QueryParameter[Option[A]] {
      override def render(key: String, value: Option[A]): String = {
        value.map(implicitly[QueryParameter[A]].render(key, _)).getOrElse("")
      }
    }
  }

  implicit def seqQueryParameter[A : QueryParameter]: QueryParameter[Seq[A]] = {
    new QueryParameter[Seq[A]] {
      override def render(key: String, value: Seq[A]): String = renderSeq(key, value)
    }
  }

  implicit def listQueryParameter[A : QueryParameter]: QueryParameter[List[A]] = {
    seqQueryParameter[A].transform(_.toSeq)
  }

  def querySeq[A : QueryParameter](separator: Char): QueryParameter[Seq[A]] = {
    new QueryParameter[Seq[A]] {
      override def render(key: String, value: Seq[A]): String = renderSeqWithSeparator(key, value, separator)
    }
  }

  def queryList[A : QueryParameter](separator: Char): QueryParameter[List[A]] =
    querySeq[A](separator).transform(_.toSeq)

}
