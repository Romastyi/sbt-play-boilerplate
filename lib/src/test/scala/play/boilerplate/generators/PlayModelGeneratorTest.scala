package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class PlayModelGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Model generator: enums support." should "Inline definition." in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("enums/schema_inline.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("enums/schema_inline.yaml").right.get
    val ctx = DefaultGeneratorContext("enums/schema_inline.yaml", "test", "")
    val gen = new PlayModelGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Reuse definition" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("enums/schema_reuse.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("enums/schema_reuse.yaml").right.get
    val ctx = DefaultGeneratorContext("enums/schema_reuse.yaml", "test", "")
    val gen = new PlayModelGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Full support" should "Parse petStore_v1.yaml" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("petStore_v1.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("petStore_v1.yaml").right.get
    val ctx = DefaultGeneratorContext("petStore_v1.yaml", "test", "")
    val gen = new PlayModelGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("petStore_v2.yaml", "test"))

    val schema = SwaggerBackend.parseSchema("petStore_v2.yaml").right.get
    val ctx = DefaultGeneratorContext("petStore_v2.yaml", "test", "")
    val gen = new PlayModelGeneratorParser().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
