package play.boilerplate.parser.model

sealed trait ResponseCode
case object DefaultResponse extends ResponseCode
final case class StatusResponse(status: Int) extends ResponseCode

case class Response(code: ResponseCode,
                    description: Option[String],
                    content: Map[String, Definition],
                    headers: Map[String, Definition]
                    /*examples: Map[String, AnyRef]*/
                   ) extends WithResolve[Response] {

  override def containsLazyRef: Boolean = {
    content.values.exists(_.containsLazyRef) ||
    headers.values.exists(_.containsLazyRef)
  }

  override def resolve(resolver: DefinitionResolver): Response = {
    copy(
      content = content.mapValues(_.resolve(resolver)),
      headers = content.mapValues(_.resolve(resolver))
    )
  }

}
