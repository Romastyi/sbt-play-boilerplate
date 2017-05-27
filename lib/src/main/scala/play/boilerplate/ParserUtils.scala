package play.boilerplate

import io.swagger.models.Swagger
import io.swagger.parser.SwaggerParser

object ParserUtils {

  def parseSwagger(fileName: String): Option[Swagger] = {
    Option(new SwaggerParser().read(fileName))
  }

}
