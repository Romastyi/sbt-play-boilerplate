package play.boilerplate.parser.backend.swagger

import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

trait ReferenceParser { self =>

  protected def findReferenceDef(schema: Schema, ref: String)
                                (implicit ctx: ParserContext): Definition = {

    val parametersRx  = """#/parameters/(.+)""".r
    val definitionsRx = """#/definitions/(.+)""".r

    ref match {
      case parametersRx(name) =>
        schema.parameters.getOrElse(name, if (ctx.refCanBeLazy) {
          LazyRefDefinition(ref)
        } else {
          throw ParserException(s"Reference item not found ($ref).")
        })
      case definitionsRx(name) =>
        schema.definitions.getOrElse(name, if (ctx.refCanBeLazy) {
          LazyRefDefinition(ref)
        } else {
          throw ParserException(s"Reference item not found ($ref).")
        })
      case _ =>
        throw ParserException(s"Unsupported reference ($ref).")
    }

  }

  class LazyReferencesResolver(schema: Schema) extends DefinitionResolver {
    override def resolveByRef(ref: String): Definition = {
      self.findReferenceDef(schema, ref)(ParserContext(refCanBeLazy = false, consumes = Nil, produces = Nil))
    }
  }

  protected def resolveLazyReferences[A <: WithResolve[A]](schema: Schema, a: A): A = {
    a.resolve(new LazyReferencesResolver(schema))
  }

}
