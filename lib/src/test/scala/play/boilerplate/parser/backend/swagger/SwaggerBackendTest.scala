package play.boilerplate.parser.backend.swagger

import org.scalatest.{FlatSpec, Matchers}

class SwaggerBackendTest extends FlatSpec with Matchers {

  "Full support" should "Parse petStore_v1.yaml" in {

    SwaggerBackend.parseSchema("petStore_v1.yaml") match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    SwaggerBackend.parseSchema("petStore_v2.yaml") match {
      case Left(cause) => throw cause
      case Right(schema) => println(schema)
    }

    true should be (true)

  }

}
