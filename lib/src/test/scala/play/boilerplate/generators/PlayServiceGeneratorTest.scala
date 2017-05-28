package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}

class PlayServiceGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Full support" should "Parse petStore_v1.yaml" in {

    val generator = new PlayServiceGenerator()
    printSyntaxString(generator.generate("petStore_v1.yaml", "test", ""))

    true should be (true)

  }

  it should "Parse petStore_v2.yaml" in {

    val generator = new PlayServiceGenerator()
    printSyntaxString(generator.generate("petStore_v2.yaml", "test", ""))

    true should be (true)

  }

}