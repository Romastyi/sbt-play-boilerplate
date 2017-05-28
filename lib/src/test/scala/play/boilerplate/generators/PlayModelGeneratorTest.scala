package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}

class PlayModelGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Model generator: enums support." should "Inline definition." in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("enums/schema_inline.yaml", "test"))

    true should be (true)

  }

  it should "Reuse definition" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("enums/schema_reuse.yaml", "test"))

    true should be (true)

  }

  "Full support" should "Parse petStore_v1.yaml" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("petStore_v1.yaml", "test"))

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val generator = new PlayModelGenerator()
    printSyntaxString(generator.generate("petStore_v2.yaml", "test"))

    true should be (true)

  }

}
