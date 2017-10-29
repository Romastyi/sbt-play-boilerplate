package play.boilerplate.parser.model

sealed abstract class Parameter(name: String, ref: Definition) extends RefDefinition(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    throw new RuntimeException("Could not instantiate abstract class Parameter.")
  }
}

final class RefParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new RefParameter(name, ref.resolve(resolver))
  }
}

object RefParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new RefParameter(definition.name, definition)
  }
}

final class BodyParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new BodyParameter(name, ref.resolve(resolver))
  }
}

object BodyParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new BodyParameter(definition.name, definition)
  }
}

final class HeaderParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new HeaderParameter(name, ref.resolve(resolver))
  }
}

object HeaderParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new HeaderParameter(definition.name, definition)
  }
}

final class PathParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new PathParameter(name, ref.resolve(resolver))
  }
}

object PathParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new PathParameter(definition.name, definition)
  }
}

final class QueryParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new QueryParameter(name, ref.resolve(resolver))
  }
}

object QueryParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new QueryParameter(definition.name, definition)
  }
}

final class FormParameter(name: String, ref: Definition) extends Parameter(name, ref) {
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new FormParameter(name, ref.resolve(resolver))
  }
}

object FormParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition): Parameter = {
    new FormParameter(definition.name, definition)
  }
}
