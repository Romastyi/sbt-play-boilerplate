package play.boilerplate.parser.backend.swagger

import org.scalatest.{FlatSpec, Matchers}

class SwaggerBackendTest extends FlatSpec with Matchers {

  "SwaggerBackend: enums support." should "Inline definition." in {

    val result = SwaggerBackend.parseSchema("enums/schema_inline.yaml")
    result match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

  it should "Reuse definition" in {

    val result = SwaggerBackend.parseSchema("enums/schema_reuse.yaml")
    result match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

  "SwaggerBackend: full support" should "Parse petStore_v1.yaml" in {

    val result = SwaggerBackend.parseSchema("petStore_v1.yaml")
    result match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val result = SwaggerBackend.parseSchema("petStore_v2.yaml")
    result match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

}
