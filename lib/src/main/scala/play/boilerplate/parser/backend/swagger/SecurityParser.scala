package play.boilerplate.parser.backend.swagger

import io.swagger.models.auth._
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait SecurityParser {

  protected def parseSecuritySchema(schemaName: String, securitySchema: SecuritySchemeDefinition): SecuritySchema = {

    securitySchema match {
      case _: BasicAuthDefinition =>
        BasicSecuritySchema(schemaName)
      case schema: ApiKeyAuthDefinition =>
        ApiKeySecuritySchema(
          schemaName = schemaName,
          headerName = Option(schema.getName).getOrElse {
            throw ParserException(s"Attribute 'name' is not specified for security schema '$schemaName'.")
          }
        )
      case schema: OAuth2Definition =>
        OAuth2SecuritySchema(
          schemaName = schemaName,
          authorizationUrl = Option(schema.getAuthorizationUrl).getOrElse {
            throw ParserException(s"Attribute 'authorizationUrl' is not specified for security schema '$schemaName'.")
          },
          tokenUrl = Option(schema.getTokenUrl)/*.getOrElse {
            throw ParserException(s"Attribute 'tokenUrl' is not specified for security schema '$schemaName'.")
          }*/,
          flow = Option(schema.getFlow).getOrElse {
            throw ParserException(s"Attribute 'flow' is not specified for security schema '$schemaName'.")
          },
          scopes = Option(schema.getScopes).map(_.asScala.toMap).getOrElse {
            throw ParserException(s"Attribute 'scopes' is not specified for security schema '$schemaName'.")
          }
        )
      case schema =>
        throw ParserException(s"Unsupported secutiry schema type (${schema.getClass.getName}).")
    }

  }

}
