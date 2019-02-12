package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class RoutesCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Empty routes" should "Parse support/empty_object_as_jsobject.yaml" in {

    val schema = SwaggerBackend.parseSchema("support/empty_object_as_jsobject.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/empty_object_as_jsobject.yaml", "test", Nil))
    val gen = InjectedRoutesCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    gen.size should be(0)

  }

  "Full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings(
      "petStore.v1.yaml",
      "test",
      Nil,
      useTraceId = true,
      traceIdHeader = Some("X-TraceID")
    ))
    val gen = DynamicRoutesCodeGenerator("api/1/").generate(schema)(ctx)
    printCodeFile(gen)

    gen.size should be(1)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v2.yaml", "test", Nil))
    val gen = DynamicRoutesCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    gen.size should be(1)

  }

}

