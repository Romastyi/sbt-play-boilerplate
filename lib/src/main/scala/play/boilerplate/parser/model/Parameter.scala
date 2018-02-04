package play.boilerplate.parser.model

sealed abstract class Parameter(name: String, desc: Option[String], ref: Definition) extends RefDefinition(name, ref) {
  override def description: Option[String] = desc orElse ref.description
  def withDesc(description: Option[String]): Parameter
  override def resolve(resolver: DefinitionResolver): Parameter = {
    throw new RuntimeException("Could not instantiate abstract class Parameter.")
  }
}

final class RefParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new RefParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new RefParameter(name, desc, ref.resolve(resolver))
  }
}

object RefParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new RefParameter(name getOrElse definition.name, None, definition)
  }
}

final class BodyParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new BodyParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new BodyParameter(name, desc, ref.resolve(resolver))
  }
}

object BodyParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new BodyParameter(name getOrElse definition.name, None, definition)
  }
}

final class HeaderParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new HeaderParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new HeaderParameter(name, desc, ref.resolve(resolver))
  }
}

object HeaderParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new HeaderParameter(name getOrElse definition.name, None, definition)
  }
}

final class PathParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new PathParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new PathParameter(name, desc, ref.resolve(resolver))
  }
}

object PathParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new PathParameter(name getOrElse definition.name, None, definition)
  }
}

final class QueryParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new QueryParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new QueryParameter(name, desc, ref.resolve(resolver))
  }
}

object QueryParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new QueryParameter(name getOrElse definition.name, None, definition)
  }
}

final class FormParameter(name: String, desc: Option[String], ref: Definition) extends Parameter(name, desc, ref) {
  override def withDesc(description: Option[String]): Parameter = {
    new FormParameter(name, description, ref)
  }
  override def resolve(resolver: DefinitionResolver): Parameter = {
    new FormParameter(name, desc, ref.resolve(resolver))
  }
}

object FormParameterFactory extends DefinitionFactory[Parameter] {
  override def build(definition: Definition, name: Option[String]): Parameter = {
    new FormParameter(name getOrElse definition.name, None, definition)
  }
}
