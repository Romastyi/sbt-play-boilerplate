package play.boilerplate.parser.model

import scala.annotation.tailrec

final class Model(name: String, ref: Definition, val isInterface: Boolean) extends RefDefinition(name, ref) {

  @tailrec
  private def findComplex(definition: Definition): Option[ComplexObjectDefinition] = definition match {
    case RefDefinition(_, reference) => findComplex(reference)
    case complex: ComplexObjectDefinition => Some(complex)
    case _ => None
  }

  lazy val complexObject: Option[ComplexObjectDefinition] = findComplex(ref)

  override def resolve(resolver: DefinitionResolver): Model = {
    new Model(name, ref.resolve(resolver), isInterface)
  }

}

object ModelFactory extends DefinitionFactory[Model] {
  override def build(definition: Definition): Model = {
    new Model(definition.name, definition, isInterface = false)
  }
}
