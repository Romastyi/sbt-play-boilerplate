package play.boilerplate.parser.model

sealed trait SecuritySchema {
  def schemaName: String
}

final case class BasicSecuritySchema(override val schemaName: String) extends SecuritySchema

final case class ApiKeySecuritySchema(override val schemaName: String,
                                      headerName: String) extends SecuritySchema

final case class OAuth2SecuritySchema(override val schemaName: String,
                                      authorizationUrl: String,
                                      tokenUrl: Option[String],
                                      flow: String,
                                      scopes: Map[String, String]
                                     ) extends SecuritySchema

final case class SecurityRequirement(schemaName: String, scopes: Iterable[String])