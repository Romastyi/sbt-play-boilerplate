package play.boilerplate.parser.model

trait WithResolve[A] {
  def containsLazyRef: Boolean
  def resolve(resolver: DefinitionResolver): A
}

trait DefinitionResolver {
  def resolveByRef(ref: String): Definition
}

