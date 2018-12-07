package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class ModelCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Model generator: enums support." should "Inline definition." in {

    val schema = SwaggerBackend.parseSchema("enums/schema_inline.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_inline.yaml", "test", "", enumGenerator = SealedTraitEnumerations))
    val gen = new ModelCodeGenerator(inOneFile = true).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Reuse definition" in {

    val schema = SwaggerBackend.parseSchema("enums/schema_reuse.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("enums/schema_reuse.yaml", "test", ""))
    val gen = new ModelCodeGenerator(inOneFile = true).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Model generator: Polymorphism support." should "Inheritance" in {

    val schema = SwaggerBackend.parseSchema("polymorphism/inheritance.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("polymorphism/inheritance.yaml", "test", ""))
    val gen = new ModelCodeGenerator(inOneFile = true).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v1.yaml", "test", ""))
    val gen = new ModelCodeGenerator(inOneFile = true).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v2.yaml", "test", ""))
    val gen = new ModelCodeGenerator(inOneFile = false).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
