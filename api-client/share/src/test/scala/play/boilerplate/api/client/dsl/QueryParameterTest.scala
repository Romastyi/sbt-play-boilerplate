package play.boilerplate.api.client.dsl

import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class QueryParameterTest extends FlatSpec with Matchers {

  "QueryParameter" should "basic types support" in {
    QueryParameter.render("param1", "abc") shouldBe "param1=abc"
    QueryParameter.render("param1", 'x') shouldBe "param1=x"
    QueryParameter.render("param1", 10) shouldBe "param1=10"
    QueryParameter.render("param1", 10l) shouldBe "param1=10"
    QueryParameter.render("param1", 10.0d) shouldBe "param1=10.0"
    QueryParameter.render("param1", 10.1f) shouldBe "param1=10.1"
    QueryParameter.render("param1", BigDecimal(10.5)) shouldBe "param1=10.5"
    QueryParameter.render("param1", true) shouldBe "param1=true"
    QueryParameter.render("param1", false) shouldBe "param1=false"
    val uuid = UUID.randomUUID()
    QueryParameter.render("param1", uuid) shouldBe ("param1=" + uuid.toString)
  }

  it should "list of values" in {
    val values = List(1,2,3)
    QueryParameter.render("listOf", values) shouldBe "listOf=1&listOf=2&listOf=3"
    QueryParameter.queryList[Int](',').render("listOf", values) shouldBe "listOf=1,2,3"
  }

  it should "case class support" in {

    final case class Pager(drop: Option[Int], limit: Long)

    implicit object PagerQueryParameter extends QueryParameter[Pager] {
      override def render(key: String, value: Pager): String = {
        QueryParameter.concatAll(Seq(
          QueryParameter.render[Option[Int]](key + ".drop", value.drop),
          QueryParameter.render[Long](key + ".limit", value.limit)
        ))
      }
    }

    QueryParameter.render("pager", Pager(None, 10)) shouldBe "pager.limit=10"
    QueryParameter.render("pager", Pager(Some(3), 50)) shouldBe "pager.drop=3&pager.limit=50"

  }

  it should "singleton object support" in {

    case object Obj

    implicit val ObjQueryParameter: QueryParameter[Obj.type] = QueryParameter[String].transform(_ => "")

    QueryParameter.render("obj", Obj) shouldBe "obj="
    QueryParameter.render("obj", Obj) shouldBe "obj="

  }

}
