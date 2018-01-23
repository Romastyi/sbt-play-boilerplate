package play.boilerplate.parser.model

case class Schema(host: String,
                  basePath: String,
                  version: Option[String],
                  description: Option[String],
                  schemes: Iterable[String],
                  paths: Iterable[Path],
                  security: Iterable[SecurityRequirement],
                  securitySchemas: Map[String, SecuritySchema],
                  definitions: Map[String, Model],
                  parameters: Map[String, Parameter],
                  requestBodies: Map[String, Request],
                  responses: Map[ResponseCode, Response]
                 ) extends WithResolve[Schema] {

  private def markInterfaces(definitions: Map[String, Model]): Map[String, Model] = {
    val complexObjects = definitions.values.flatMap(_.complexObject)
    if (complexObjects.nonEmpty) {
      for ((name, model) <- definitions) yield {
        if (model.complexObject.isEmpty) {
          val isIntf = complexObjects.foldLeft(false) {
            case (false, complex) => complex.hasInterface(model.ref)
            case (true, _) => true
          }
          name -> new Model(name, model.ref, isInterface = isIntf)
        } else {
          name -> model
        }
      }
    } else {
      definitions
    }
  }

  override def containsLazyRef: Boolean = {
    definitions.values.exists(_.containsLazyRef) ||
    parameters.values.exists(_.containsLazyRef) ||
    requestBodies.values.exists(_.containsLazyRef) ||
    responses.values.exists(_.containsLazyRef)
  }

  override def resolve(resolver: DefinitionResolver): Schema = {
    copy(
      definitions   = markInterfaces(definitions.mapValues(_.resolve(resolver))),
      parameters    = parameters.mapValues(_.resolve(resolver)),
      requestBodies = requestBodies.mapValues(_.resolve(resolver)),
      responses     = responses.mapValues(_.resolve(resolver))
    )
  }

}

object Schema {

  def empty: Schema = {
    Schema(
      host = "localhost",
      basePath = "/",
      version  = None,
      description = None,
      schemes  = Nil,
      paths    = Nil,
      security = Nil,
      securitySchemas = Map.empty,
      definitions     = Map.empty,
      parameters      = Map.empty,
      requestBodies   = Map.empty,
      responses       = Map.empty
    )
  }

}