package play.boilerplate.parser.model

case class Operation(httpMethod: HttpMethod.Value,
                     operationId: String,
                     parameters: Iterable[Parameter],
                     schemes: Iterable[String],
                     requestBody: Option[Request],
                     responses: Map[ResponseCode, Response],
                     description: Option[String],
                     security: Iterable[SecurityRequirement],
                     deprecated: Boolean
                    )
