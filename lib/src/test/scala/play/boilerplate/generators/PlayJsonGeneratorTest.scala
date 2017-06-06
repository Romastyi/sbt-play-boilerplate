package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class PlayJsonGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Json generator: enums support." should "Inline definition." in {

    val generator = new PlayJsonGenerator()
    printSyntaxString(generator.generate("enums/schema_inline.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("enums/schema_inline.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_inline.yaml", "test", ""))
    val gen = new PlayJsonGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Reuse definition" in {

    val generator = new PlayJsonGenerator()
    printSyntaxString(generator.generate("enums/schema_reuse.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("enums/schema_reuse.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_reuse.yaml", "test", ""))
    val gen = new PlayJsonGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Full support" should "Parse petStore_v1.yaml" in {

    val generator = new PlayJsonGenerator()
    printSyntaxString(generator.generate("petStore_v1.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("petStore_v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore_v1.yaml", "test", ""))
    val gen = new PlayJsonGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val generator = new PlayJsonGenerator()
    printSyntaxString(generator.generate("petStore_v2.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("petStore_v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore_v2.yaml", "test", ""))
    val gen = new PlayJsonGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
