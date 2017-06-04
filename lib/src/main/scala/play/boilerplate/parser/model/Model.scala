package play.boilerplate.parser.model

final class Model(name: String, ref: Definition) extends RefDefinition(name, ref) {
  override def resolve(resolver: DefinitionResolver): Model = {
    new Model(name, ref.resolve(resolver))
  }
}

object ModelFactory extends DefinitionFactory[Model] {
  override def build(definition: Definition): Model = {
    new Model(definition.name, definition)
  }
}
