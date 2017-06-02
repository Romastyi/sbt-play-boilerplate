package play.boilerplate.parser.backend

import org.scalatest.{FlatSpec, Matchers}

class SwaggerBackendTest extends FlatSpec with Matchers {

  "Full support" should "Parse petStore_v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore_v1.yaml")
    println(schema.right.get)

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore_v2.yaml")
    println(schema.right.get)

    true should be (true)

  }

}
