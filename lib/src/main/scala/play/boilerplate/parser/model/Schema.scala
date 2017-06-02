package play.boilerplate.parser.model

case class Schema(host: String,
                  basePath: String,
                  schemes: Iterable[String],
                  consumes: Iterable[String],
                  produces: Iterable[String],
                  //security: List[SecurityRequirement],
                  paths: Iterable[Path],
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
      schemes  = Nil,
      consumes = Nil,
      produces = Nil,
      //security: List[SecurityRequirement],
      paths = Nil,
      securitySchemas = Map.empty,
      definitions = Map.empty,
      parameters  = Map.empty,
      responses   = Map.empty
    )
  }

}