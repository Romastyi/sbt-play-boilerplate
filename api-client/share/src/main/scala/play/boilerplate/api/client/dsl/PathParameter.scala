package play.boilerplate.api.client.dsl

import java.util.UUID

trait PathParameter[A] { self =>

  def render(value: A): String

  def transform[B](f: B => A): PathParameter[B] = new PathParameter[B] {
    override def render(value: B): String = self.render(f(value))
  }

}

object PathParameter {

  def apply[A : PathParameter]: PathParameter[A] = implicitly[PathParameter[A]]

  def render[A : PathParameter](value: A): String = apply[A].render(value)

  implicit object stringPathParameter extends PathParameter[String] {
    override def render(value: String): String = value
  }

  implicit val charPathParameter: PathParameter[Char] =
    PathParameter[String].transform(_.toString)

  implicit val intPathParameter: PathParameter[Int] =
    PathParameter[String].transform(_.toString)

  implicit val longPathParameter: PathParameter[Long] =
    PathParameter[String].transform(_.toString)

  implicit val doublePathParameter: PathParameter[Double] =
    PathParameter[String].transform(_.toString)

  implicit val floatPathParameter: PathParameter[Float] =
    PathParameter[String].transform(_.toString)

  implicit val bigDecimalPathParameter: PathParameter[BigDecimal] =
    PathParameter[String].transform(_.toString())

  implicit val booleanPathParameter: PathParameter[Boolean] =
    PathParameter[String].transform(_.toString)

  implicit val uuidPathParameter: PathParameter[UUID] =
    PathParameter[String].transform(_.toString)

}
