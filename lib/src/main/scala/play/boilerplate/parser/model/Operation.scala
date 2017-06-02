package play.boilerplate.parser.model

case class Operation(httpMethod: HttpMethod.Value,
                     operationId: String,
                     parameters: Iterable[Definition with Parameter],
                     schemes: Iterable[String],
                     consumes: Iterable[String],
                     produces: Iterable[String],
                     responses: Map[ResponseCode, Response],
                     description: Option[String],
                     security: Iterable[Map[String, Iterable[String]]],
                     deprecated: Boolean
                    )
