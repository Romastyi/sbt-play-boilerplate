package play.boilerplate.parser.model

case class Schema(host: String,
                  basePath: String,
                  version: Option[String],
                  description: Option[String],
                  schemes: Iterable[String],
                  consumes: Iterable[String],
                  produces: Iterable[String],
                  paths: Iterable[Path],
                  security: Iterable[SecurityRequirement],
                  securitySchemas: Map[String, SecuritySchema],
                  definitions: Map[String, Definition with Model],
                  parameters: Map[String, Definition with Parameter],
                  responses: Map[ResponseCode, Response]
                 )

object Schema {

  def empty: Schema = {
    Schema(
      host = "localhost",
      basePath = "/",
      version  = None,
      description = None,
      schemes  = Nil,
      consumes = Nil,
      produces = Nil,
      paths = Nil,
      security = Nil,
      securitySchemas = Map.empty,
      definitions = Map.empty,
      parameters  = Map.empty,
      responses   = Map.empty
    )
  }

}