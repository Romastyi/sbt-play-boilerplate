package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.generators.support.CustomTypeSupport
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class JsonCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Json generator: enums support." should "Inline definition." in {

    val schema = SwaggerBackend.parseSchema("enums/schema_inline.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_inline.yaml", "test", ""))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Reuse definition" in {

    val schema = SwaggerBackend.parseSchema("enums/schema_reuse.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_reuse.yaml", "test", ""))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "More than 22 fields case class support." in {

    val schema = SwaggerBackend.parseSchema("support/more-than-22-fields.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/more-than-22-fields.yaml", "test", ""))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Json generator: Polymorphism support." should "Inheritance" in {

    val schema = SwaggerBackend.parseSchema("polymorphism/inheritance.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("polymorphism/inheritance.yaml", "test", ""))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v1.yaml", "test", ""))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings(
      "petStore.v2.yaml", "test", "",
      customTypeSupport = {
        CustomTypeSupport.jodaLocalDateSupport() ++
        CustomTypeSupport.jodaDateTimeSupport()
      }
    ))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
