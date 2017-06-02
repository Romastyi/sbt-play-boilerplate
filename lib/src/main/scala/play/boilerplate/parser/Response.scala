package play.boilerplate.parser

sealed trait ResponseCode
case object DefaultResponse
final case class ResponseStatus(status: Int) extends ResponseCode

case class Response(code: ResponseCode,
                    schema: Definition with Property,
                    headers: Map[String, Definition with Property]
                    /*examples: Map[String, AnyRef]*/
                   )
