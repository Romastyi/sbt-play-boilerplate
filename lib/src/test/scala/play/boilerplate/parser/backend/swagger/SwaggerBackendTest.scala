package play.boilerplate.parser.backend.swagger

import org.scalatest.{FlatSpec, Matchers}

class SwaggerBackendTest extends FlatSpec with Matchers {

  "Full support" should "Parse petStore_v1.yaml" in {

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
