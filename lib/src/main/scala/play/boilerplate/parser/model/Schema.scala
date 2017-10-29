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
                  definitions: Map[String, Model],
                  parameters: Map[String, Parameter],
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
    responses.values.exists(_.containsLazyRef)
  }

  override def resolve(resolver: DefinitionResolver): Schema = {
    copy(
      definitions = markInterfaces(for ((name, model) <- definitions) yield {
        name -> model.resolve(resolver)
      }),
      parameters = for ((name, param) <- parameters) yield {
        name -> param.resolve(resolver)
      },
      responses = for ((code, resp) <- responses) yield {
        code -> resp.resolve(resolver)
      }
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
      consumes = Nil,
      produces = Nil,
      paths    = Nil,
      security = Nil,
      securitySchemas = Map.empty,
      definitions = Map.empty,
      parameters  = Map.empty,
      responses   = Map.empty
    )
  }

}