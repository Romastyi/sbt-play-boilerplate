package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.generators.security.{Play2AuthSecurityProvider, SecurityProvider}
import play.boilerplate.parser.backend.swagger.SwaggerBackend
import treehugger.forest

class ClientCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Full support" should "Parse petStore.v1.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val security = new Play2AuthSecurityProvider("User", "AuthConfig", "session") {
      override def parseAuthority(scopes: Seq[SecurityProvider.SecurityScope]): Seq[forest.Tree] = Nil
    }
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v1.yaml", "test", "", injectionProvider = injection.ScaldiInjectionProvider, securityProviders = List(security)))
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