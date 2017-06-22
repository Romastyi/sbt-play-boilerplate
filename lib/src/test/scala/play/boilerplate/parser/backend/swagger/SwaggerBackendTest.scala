package play.boilerplate.parser.backend.swagger

import org.scalatest.{FlatSpec, Matchers}

class SwaggerBackendTest extends FlatSpec with Matchers {

  "SwaggerBackend: enums support." should "Inline definition." in {

    val schema = SwaggerBackend.parseSchema("enums/schema_inline.yaml").get
    println(schema)

    true should be (true)

  }

  it should "Reuse definition" in {

    val schema = SwaggerBackend.parseSchema("enums/schema_reuse.yaml").get
    println(schema)

    true should be (true)

  }

  "SwaggerBackend: full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    println(schema)

    true should be (true)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    println(schema)

    true should be (true)

  }

}
