package play.boilerplate.generators

import org.scalatest.{FlatSpec, Matchers}
import play.boilerplate.generators.GeneratorUtils._
import play.boilerplate.generators.logger.LoggerProvider
import play.boilerplate.generators.security.{Play2AuthSecurityProvider, SecurityProvider}
import play.boilerplate.parser.backend.swagger.SwaggerBackend

class ControllerCodeGeneratorTest extends FlatSpec with Matchers with PrintSyntaxString {

  "Controller generator: Polymorphism support." should "Inheritance" in {

    val schema = SwaggerBackend.parseSchema("polymorphism/inheritance.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("polymorphism/inheritance.yaml", "test", Nil, loggerProvider = LoggerProvider.withoutLogger))
    val gen = new ControllerCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  "Full support" should "Parse petStore.v1.yaml" in {

    import treehugger.forest._
    import treehuggerDSL._

    val schema = SwaggerBackend.parseSchema("petStore.v1.yaml").get
    val security = new Play2AuthSecurityProvider("User", "AuthConfig", "session") {
      override def parseAuthority(scopes: Seq[SecurityProvider.SecurityScope]): Seq[Tree] = Nil
    }
    val mimeTypeSupport = Map(
      MIME_TYPE_TEXT -> MimeTypeSupport(MIME_TYPE_TEXT, REQUEST_AS_TEXT, tpe => ident => tpe APPLY ident, _ => ident => ident DOT "toString()", identity)
    )
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings(
      "petStore.v1.yaml",
      "test",
      Nil,
      injectionProvider = injection.ScaldiInjectionProvider,
      securityProviders = List(security),
      supportedMimeTypes = mimeTypeSupport,
      strictAcceptHeaderCheck = true,
      useTraceId = true,
      traceIdHeader = Some("X-TraceID")
    ))
    val gen = new ControllerCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

  it should "Parse petStore.v2.yaml" in {

    val schema = SwaggerBackend.parseSchema("petStore.v2.yaml").get
    val ctx = GeneratorContext.initial(DefaultGeneratorSettings("petStore.v2.yaml", "test", Nil))
    val gen = new ControllerCodeGenerator().generate(schema)(ctx)
    printCodeFile(gen)

    true should be (true)

  }

}
