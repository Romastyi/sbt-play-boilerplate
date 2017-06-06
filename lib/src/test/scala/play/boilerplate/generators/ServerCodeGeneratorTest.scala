package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class ServerCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Full support" should "Parse petStore_v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore_v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore_v1.yaml", "test", ""))
    val gen = new ServerCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore_v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore_v2.yaml", "test", ""))
    val gen = new ServerCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
