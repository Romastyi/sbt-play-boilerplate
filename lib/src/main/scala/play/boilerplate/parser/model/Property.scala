package play.boilerplate.parser.model

final class Property(name: String, ref: Definition) extends RefDefinition(name, ref) {
  override def resolve(resolver: DefinitionResolver): Property = {
    new Property(name, ref.resolve(resolver))
  }
}

object PropertyFactory extends DefinitionFactory[Property] {
  override def build(definition: Definition, name: Option[String]): Property = {
    new Property(name getOrElse definition.name, definition)
  }
}
