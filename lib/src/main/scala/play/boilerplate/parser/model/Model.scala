package play.boilerplate.parser.model

final class Model(name: String, ref: Definition, val isInterface: Boolean) extends RefDefinition(name, ref) {

  private def findComplex(definition: Definition): Option[ComplexObjectDefinition] = {
    definition.baseDef match {
      case complex: ComplexObjectDefinition => Some(complex)
      case _ => None
    }
  }

  lazy val complexObject: Option[ComplexObjectDefinition] = findComplex(ref)

  override def resolve(resolver: DefinitionResolver): Model = {
    new Model(name, ref.resolve(resolver), isInterface)
  }

}

object ModelFactory extends DefinitionFactory[Model] {
  override def build(definition: Definition, name: Option[String]): Model = {
    new Model(name getOrElse definition.name, definition, isInterface = false)
  }
}
