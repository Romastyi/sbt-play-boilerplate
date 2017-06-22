package play.boilerplate.generators.support

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.generators._
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class CustomTypeSupportTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Empty object as JsObject:" should "model" in {

    val schema = SwaggerBackend.parseSchema("support/empty_object_as_jsobject.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/empty_object_as_jsobject.yaml", "test", "").copy(
      customTypeSupport = CustomTypeSupport.emptyObjectAsJsObject
    ))
    val gen = new ModelCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "json" in {

    val schema = SwaggerBackend.parseSchema("support/empty_object_as_jsobject.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/empty_object_as_jsobject.yaml", "test", "").copy(
      customTypeSupport = CustomTypeSupport.emptyObjectAsJsObject
    ))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
