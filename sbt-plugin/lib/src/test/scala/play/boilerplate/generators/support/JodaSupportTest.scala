package play.boilerplate.generators.support

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.generators._
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class JodaSupportTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Joda types support" should "model" in {

    val schema = SwaggerBackend.parseSchema("support/joda-support.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/joda-support.yaml", "test", Nil).copy(
      customTypeSupport = {
        CustomTypeSupport.jodaLocalDateSupport() ++
        CustomTypeSupport.jodaDateTimeSupport()
      }
    ))
    val gen = new ModelCodeGenerator(inOneFile = true).generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "json" in {

    val schema = SwaggerBackend.parseSchema("support/joda-support.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/joda-support.yaml", "test", Nil).copy(
      customTypeSupport = {
        CustomTypeSupport.jodaLocalDateSupport() ++
        CustomTypeSupport.jodaDateTimeSupport()
      }
    ))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "json with corrector" in {

    val schema = SwaggerBackend.parseSchema("support/joda-support.yaml").get
    val dateCorrector = {
      import treehugger.forest._
      import definitions._
      import treehuggerDSL._
      val StringUtils = RootClass.newClass("org.apache.commons.lang3.StringUtils")
      LAMBDA(PARAM("s").tree) ==> (StringUtils DOT "substringBefore" APPLY(
        StringUtils DOT "substringBefore" APPLY(REF("s"), LIT("T")), LIT(" ")
      ))
    }
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("support/joda-support.yaml", "test", Nil).copy(
      customTypeSupport = {
        CustomTypeSupport.jodaLocalDateSupport(corrector = dateCorrector) ++
        CustomTypeSupport.jodaDateTimeSupport()
      }
    ))
    val gen = new JsonCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
