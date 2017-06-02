package play.boilerplate.parser.backend

import io.swagger.models.Swagger
import io.swagger.models.{Path => SwaggerPath}
import io.swagger.models.{Operation => SwaggerOperation}
import io.swagger.models.parameters.{AbstractSerializableParameter, Parameter => SwaggerParameter}
import io.swagger.models.properties.{ArrayProperty, PropertyBuilder, Property => SwaggerProperty}
import io.swagger.parser.SwaggerParser
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object SwaggerBackend extends ParserBackend {

  override def parseSchema(fileName: String): Either[ParserException, Schema] = {

    (for {
      swagger <- Try(new SwaggerParser().read(fileName))
      schema = Option(swagger).map(internalParse).getOrElse {
        throw new RuntimeException(s"Parsing schema file is failed ($fileName).")
      }
    } yield schema) match {
      case Success(model) => Right(model)
      case Failure(cause) => Left(ParserException(cause.getMessage, cause))
    }

  }

  private def internalParse(swagger: Swagger): Schema = {

    val parameters = Option(swagger.getParameters).map(_.asScala.toMap).getOrElse(Map.empty)
      .map { case (name, param) =>
        param.setName(name)
        name -> parseParameter(param)
      }

    val initial = Schema(
      host     = Option(swagger.getHost).getOrElse("localhost"),
      basePath = Option(swagger.getBasePath).getOrElse("/"),
      schemes  = Option(swagger.getSchemes).map(_.asScala).getOrElse(Nil).map(_.toValue),
      consumes = Option(swagger.getConsumes).map(_.asScala).getOrElse(Nil),
      produces = Option(swagger.getProduces).map(_.asScala).getOrElse(Nil),
      //security: List[SecurityRequirement],
      paths = Nil,
      //securityDefinitions: Map[String, SecuritySchemeDefinition],
      definitions = Map.empty,
      parameters = parameters,
      responses = Map.empty
    )

    initial.copy(
      paths = parsePaths(swagger, initial)
    )

  }

  private def parsePaths(swagger: Swagger, schema: Schema): Iterable[Path] = {

    for {
      (url, path) <- Option(swagger.getPaths.asScala.toMap).getOrElse(Map.empty)
    } yield {
      Path(
        pathUrl = url,
        pathParts = parsePathUrl(url),
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

  private def parsePathOperations(schema: Schema, pathUrl: String, path: SwaggerPath): Map[HttpMethod.Value, Operation] = {
    Seq(
      Option(path.getGet   ).map(HttpMethod.Get    -> parseOperation(schema, pathUrl, HttpMethod.Get   , _)),
      Option(path.getPut   ).map(HttpMethod.Put    -> parseOperation(schema, pathUrl, HttpMethod.Put   , _)),
      Option(path.getPost  ).map(HttpMethod.Post   -> parseOperation(schema, pathUrl, HttpMethod.Post  , _)),
      Option(path.getDelete).map(HttpMethod.Delete -> parseOperation(schema, pathUrl, HttpMethod.Delete, _))
    ).flatten.toMap
  }

  private def parseOperation(schema: Schema, pathUrl: String, httpMethod: HttpMethod.Value, operation: SwaggerOperation): Operation = {

    Operation(
      httpMethod = httpMethod,
      operationId = Option(operation.getOperationId).filter(_.nonEmpty).getOrElse {
        throw new RuntimeException(s"Attribute 'operationId' id not specified for path '$pathUrl' and method '${httpMethod.toString.toLowerCase()}'.")
      },
      parameters = Option(operation.getParameters).map(_.asScala).getOrElse(Nil).map(parseParameter),
      schemes  = Option(operation.getSchemes).map(_.asScala.map(_.toValue)).getOrElse(schema.schemes),
      consumes = Option(operation.getConsumes).map(_.asScala).getOrElse(schema.consumes),
      produces = Option(operation.getProduces).map(_.asScala).getOrElse(schema.produces),
      responses = Map.empty,
      description = Option(operation.getDescription),
      security = Nil,
      deprecated = Option(operation.isDeprecated).exists(_ == true)
    )

  }

  private def parseParameter(parameter: SwaggerParameter): Definition with Parameter = {

    import io.swagger.models.{parameters => swagger}

    parameter match {
      case bp: swagger.BodyParameter =>
        ???
      case hp: swagger.HeaderParameter =>
        parseTypedParameter(hp)
      case pp: swagger.PathParameter =>
        parseTypedParameter(pp)
      case qp: swagger.QueryParameter =>
        parseTypedParameter(qp)
      case fp: swagger.RefParameter =>
        ???
    }

  }

  object TypedParameter {
    def unapply(arg: Parameter): Option[AbstractSerializableParameter[_]] = {
      arg match {
        case param: AbstractSerializableParameter[_] =>
          Some(param)
        case _ =>
          None
      }
    }
  }

  object EnumParameter {
    def unapply(arg: Parameter): Option[(AbstractSerializableParameter[_], Iterable[String])] = {
      arg match {
        case TypedParameter(param) if Option(param.getEnum).isDefined =>
          Some((param, param.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  object ArrayParameter {
    def unapply(arg: Parameter): Option[(AbstractSerializableParameter[_], SwaggerProperty)] = {
      arg match {
        case TypedParameter(param) if param.getType == ArrayProperty.TYPE =>
          Some((param, param.getItems))
        case _ =>
          None
      }
    }
  }

  private def parseTypedParameter(parameter: AbstractSerializableParameter[_]): Definition with Parameter = {
    parameter match {
      case ArrayParameter(ap, prop) =>
        ???
      case EnumParameter(ep, items) =>
        ???
      case param =>
        ???
    }
  }

  private def param2Prop(parameter: AbstractSerializableParameter[_]): SwaggerProperty = {

    import io.swagger.models.properties.PropertyBuilder.PropertyId._

    val args = Map(
      ENUM -> parameter.getEnum,
      DESCRIPTION -> parameter.getDescription,
      DEFAULT -> parameter.getDefault,
      PATTERN -> parameter.getPattern,
      MIN_ITEMS -> parameter.getMinItems,
      MAX_ITEMS -> parameter.getMaxItems,
      MIN_LENGTH -> parameter.getMinLength,
      MAX_LENGTH -> parameter.getMaxLength,
      MINIMUM -> parameter.getMinimum,
      MAXIMUM -> parameter.getMaximum,
      EXAMPLE -> parameter.getExample,
      TYPE -> parameter.getType,
      FORMAT -> parameter.getFormat,
      REQUIRED -> boolean2Boolean(Option(parameter.getRequired).getOrElse(false)),
      VENDOR_EXTENSIONS -> parameter.getVendorExtensions,
      ALLOW_EMPTY_VALUE -> parameter.getAllowEmptyValue,
      MULTIPLE_OF -> parameter.getMultipleOf
    ).asJava

    PropertyBuilder.build(parameter.getType, parameter.getFormat, args)

  }

}
