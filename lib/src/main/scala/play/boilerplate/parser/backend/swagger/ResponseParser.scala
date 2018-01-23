package play.boilerplate.parser.backend.swagger

import io.swagger.models.{Response => SwaggerResponse}
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ResponseParser { this: PropertyParser =>

  protected def parseResponse(schema: Schema, code: String, produces: Iterable[String], response: SwaggerResponse)
                             (implicit ctx: ParserContext): (ResponseCode, Response) = {

    val statusRx = """(\d+)""".r

    val respCode: ResponseCode = code match {
      case "default"   => DefaultResponse
      case statusRx(s) => StatusResponse(s.toInt)
      case _ => throw ParserException(s"Invalid response code ($code).")
    }

    val headers = Option(response.getHeaders)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .map { case (name, prop) =>
        name -> getPropertyDef(schema, name, prop)
      }

    val schemaDef = Option(response.getSchema).map { model =>
      getPropertyDef(schema, code, model, canBeOption = false)
    }

    respCode -> Response(
      code = respCode,
      description = Option(response.getDescription),
      content = (produces zip schemaDef).toMap,
      headers = headers
    )

  }

}
