package play.boilerplate.parser.backend.swagger

import io.swagger.models.{Operation => SwaggerOperation, Path => SwaggerPath}
import play.boilerplate.parser.model.{HttpMethod, Operation, Schema}

import scala.collection.JavaConverters._

trait OperationParser { this: ParameterParser with ResponseParser =>

  protected def parsePathOperations(schema: Schema, pathUrl: String, path: SwaggerPath): Map[HttpMethod.Value, Operation] = {
    Seq(
      Option(path.getGet   ).map(HttpMethod.Get    -> parseOperation(schema, pathUrl, HttpMethod.Get   , _)),
      Option(path.getPut   ).map(HttpMethod.Put    -> parseOperation(schema, pathUrl, HttpMethod.Put   , _)),
      Option(path.getPost  ).map(HttpMethod.Post   -> parseOperation(schema, pathUrl, HttpMethod.Post  , _)),
      Option(path.getDelete).map(HttpMethod.Delete -> parseOperation(schema, pathUrl, HttpMethod.Delete, _))
    ).flatten.toMap
  }

  private def parseOperation(schema: Schema,
                             pathUrl: String,
                             httpMethod: HttpMethod.Value,
                             operation: SwaggerOperation): Operation = {

    Operation(
      httpMethod = httpMethod,
      operationId = Option(operation.getOperationId).filter(_.nonEmpty).getOrElse {
        throw new RuntimeException(s"Attribute 'operationId' id not specified for path '$pathUrl' and method '${httpMethod.toString.toLowerCase()}'.")
      },
      parameters = Option(operation.getParameters).map(_.asScala).getOrElse(Nil).map(parseParameter(schema, _)),
      schemes    = Option(operation.getSchemes).map(_.asScala.map(_.toValue)).getOrElse(schema.schemes),
      consumes   = Option(operation.getConsumes).map(_.asScala).getOrElse(schema.consumes),
      produces   = Option(operation.getProduces).map(_.asScala).getOrElse(schema.produces),
      responses  = Option(operation.getResponses).map(_.asScala.toMap.map {
        case (code, response) => parseResponse(schema, code, response)
      }).getOrElse(schema.responses),
      description = Option(operation.getDescription),
      security   = Nil,
      deprecated = Option(operation.isDeprecated).exists(_ == true)
    )

  }

}
