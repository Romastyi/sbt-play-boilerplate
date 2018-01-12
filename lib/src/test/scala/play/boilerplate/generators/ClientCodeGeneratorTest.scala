package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class ClientCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v1.yaml", "test", "", injectionProvider = injection.ScaldiInjectionProvider))
    val gen = new ClientCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v2.yaml", "test", "", injectionProvider = injection.GuiceInjectionProvider))
    val gen = new ClientCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}