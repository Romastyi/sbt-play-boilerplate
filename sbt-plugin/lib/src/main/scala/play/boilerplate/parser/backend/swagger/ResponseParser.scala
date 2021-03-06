package play.boilerplate.parser.backend.swagger

import io.swagger.models.utils.PropertyModelConverter
import io.swagger.models.{Response => SwaggerResponse}
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ResponseParser { this: PropertyParser =>

  protected def parseResponse(schema: Schema, code: String, response: SwaggerResponse)
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
        name -> HeaderParameterFactory.build(getPropertyDef(schema, name, prop), Some(name))
      }

    respCode -> Response(
      code = respCode,
      description = Option(response.getDescription),
      schema = Option(response.getResponseSchema).map { model =>
        val property = new PropertyModelConverter().modelToProperty(model)
        val responseName = respCode match {
          case DefaultResponse => "Default"
          case StatusResponse(intCode) => HttpStatus.getStatusByCode(intCode)
        }
        getPropertyDef(schema, responseName, property, canBeOption = false)
      },
      headers = headers
    )

  }

}
