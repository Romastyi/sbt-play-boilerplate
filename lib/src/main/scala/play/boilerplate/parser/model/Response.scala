package play.boilerplate.parser.model

sealed trait ResponseCode
case object DefaultResponse extends ResponseCode
final case class StatusResponse(status: Int) extends ResponseCode

case class Response(code: ResponseCode,
                    schema: Option[Definition],
                    headers: Map[String, Definition]
                    /*examples: Map[String, AnyRef]*/
                   )
