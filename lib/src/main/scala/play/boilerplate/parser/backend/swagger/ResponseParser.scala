package play.boilerplate.parser.backend.swagger

import io.swagger.models.{Response => SwaggerResponse}
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ResponseParser { this: PropertyParser =>

  protected def parseResponse(schema: Schema, code: String, response: SwaggerResponse): (ResponseCode, Response) = {

    val statusRx = """(\d+)""".r

    val respCode: ResponseCode = code match {
      case "default"   => DefaultResponse
      case statusRx(s) => StatusResponse(s.toInt)
      case _ => throw new RuntimeException(s"Invalid response code ($code).")
    }

    val headers = Option(response.getHeaders)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .map { case (name, prop) =>
        name -> getPropertyDef(schema, prop)
      }

    respCode -> Response(
      code = respCode,
      schema = Option(response.getSchema).map(getPropertyDef(schema, _)),
      headers = headers
    )

  }

}
