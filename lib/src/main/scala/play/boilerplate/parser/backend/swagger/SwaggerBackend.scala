package play.boilerplate.parser.backend.swagger

import io.swagger.models.Swagger
import io.swagger.parser.SwaggerParser
import play.boilerplate.parser.backend.{ParserBackend, ParserException}
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object SwaggerBackend
  extends ParserBackend
    with OperationParser
    with ResponseParser
    with ModelParser
    with SecurityParser
    with ParameterParser
    with PropertyParser
    with ReferenceParser {

  override def parseSchema(fileName: String): Try[Schema] = {

    for {
      swagger <- Try(new SwaggerParser().read(fileName))
      schema = Option(swagger).map(parseSwagger).getOrElse {
        throw ParserException(s"Parsing schema file is failed ($fileName).")
      }
    } yield schema

  }

  private def parseSwagger(swagger: Swagger): Schema = {

    val schema = initSchema(swagger)

    var resolved = schema

    while (resolved.containsLazyRef) {
      resolved = resolveLazyReferences(resolved, resolved)
    }

    resolved.copy(
      paths = parsePaths(swagger, resolved)(ParserContext(false))
    )

  }

  private def initSchema(swagger: Swagger): Schema = {

    implicit val ctx: ParserContext = ParserContext.initial

    val definitions = Option(swagger.getDefinitions)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .map { case (name, model) =>
        name -> parseModel(Schema.empty, name, model)
      }

    val parameters = Option(swagger.getParameters)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .map { case (name, param) =>
        param.setName(name)
        name -> parseParameter(Schema.empty, param)
      }

    val responses = Option(swagger.getResponses)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .flatMap { case (code, response) =>
        Option(response).map(parseResponse(Schema.empty, code, _))
      }

    val securitySchemas = Option(swagger.getSecurityDefinitions)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .flatMap { case (name, schema) =>
        Option(schema).map(name -> parseSecuritySchema(name, _))
      }

    Schema(
      host     = Option(swagger.getHost).getOrElse("localhost"),
      basePath = Option(swagger.getBasePath).getOrElse("/"),
      version  = Option(swagger.getInfo).flatMap(i => Option(i.getVersion)),
      description = Option(swagger.getInfo).flatMap(i => Option(i.getDescription)),
      schemes  = Option(swagger.getSchemes).map(_.asScala).getOrElse(Nil).map(_.toValue),
      consumes = Option(swagger.getConsumes).map(_.asScala).getOrElse(Nil),
      produces = Option(swagger.getProduces).map(_.asScala).getOrElse(Nil),
      paths    = Nil,
      security = parseSecurityRequirement(swagger),
      securitySchemas = securitySchemas,
      definitions = definitions,
      parameters  = parameters,
      responses   = responses
    )

  }

  private def parsePaths(swagger: Swagger, schema: Schema)
                        (implicit ctx: ParserContext): Iterable[Path] = {

    for {
      (url, path) <- Option(swagger.getPaths).map(_.asScala.toMap).getOrElse(Map.empty)
    } yield {
      Path(
        pathUrl = url,
        pathParts = parsePathUrl(url),
        parameters = Option(path.getParameters).map(_.asScala).getOrElse(Nil)
          .map(parseParameter(schema, _)),
        operations = parsePathOperations(schema, url, path)
      )
    }

  }

  private def parsePathUrl(url: String): Iterable[PathPart] = {

    val paramRx = """\{(.+)\}""".r

    url.split('/').filter(_.nonEmpty).map {
      case paramRx(name) => ParamPart(name)
      case s => StaticPart(s)
    }

  }

  private def parseSecurityRequirement(swagger: Swagger): Iterable[SecurityRequirement] = {
    Option(swagger.getSecurity).map { security =>
      for {
        auth <- security.asScala
        (name, scopes) <- Option(auth.getRequirements).map(_.asScala.toMap).getOrElse(Map.empty)
      } yield SecurityRequirement(name, scopes.asScala)
    }.getOrElse(Nil)
  }

}
