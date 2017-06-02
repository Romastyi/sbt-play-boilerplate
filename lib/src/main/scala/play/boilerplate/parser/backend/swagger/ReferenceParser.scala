package play.boilerplate.parser.backend.swagger

import play.boilerplate.parser.model.{Definition, Schema}

trait ReferenceParser {

  protected def findReferenceDef(schema: Schema, ref: String): Definition = {

    val parametersRx  = """#/parameters/(.+)""".r
    val definitionsRx = """#/definitions/(.+)""".r

    ref match {
      case parametersRx(name) =>
        schema.parameters.getOrElse(name, {
          throw new RuntimeException(s"Reference item not found ($ref).")
        })
      case definitionsRx(name) =>
        schema.definitions.getOrElse(name, {
          throw new RuntimeException(s"Reference item not found ($ref).")
        })
      case _ =>
        throw new RuntimeException(s"Unsupported reference ($ref).")
    }

  }

}
