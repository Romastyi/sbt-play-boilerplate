package play.boilerplate.api.client.dsl

import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class PathParameterTest extends FlatSpec with Matchers {

  "PathParameter" should "basic support" in {
    PathParameter.render("abc") shouldBe "abc"
    PathParameter.render('x') shouldBe "x"
    PathParameter.render(10) shouldBe "10"
    PathParameter.render(10l) shouldBe "10"
    PathParameter.render(10.0d) shouldBe "10.0"
    PathParameter.render(10.1d) shouldBe "10.1"
    PathParameter.render(BigDecimal(10.5)) shouldBe "10.5"
    PathParameter.render(true) shouldBe "true"
    PathParameter.render(false) shouldBe "false"
    val uuid = UUID.randomUUID()
    PathParameter.render(uuid) shouldBe uuid.toString
  }

}
