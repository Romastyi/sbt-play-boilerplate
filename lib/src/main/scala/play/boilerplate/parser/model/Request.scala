package play.boilerplate.parser.model

case class Request(description: Option[String],
                   content: Map[String, Definition]
                  ) extends WithResolve[Response] {

  override def containsLazyRef: Boolean = {
    content.values.exists(_.containsLazyRef)
  }

  override def resolve(resolver: DefinitionResolver): Request = {
    copy(
      content = content.mapValues(_.resolve(resolver))
    )
  }

}
