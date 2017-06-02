package play.boilerplate.parser

case class Schema(host: String,
                  basePath: String,
                  schemes: Iterable[String],
                  consumes: Iterable[String],
                  produces: Iterable[String],
                  //security: List[SecurityRequirement],
                  paths: Iterable[Path],
                  //securityDefinitions: Map[String, SecuritySchemeDefinition],
                  definitions: Map[String, Definition with Model],
                  parameters: Map[String, Definition with Parameter],
                  responses: Map[String, Response]
                 )
