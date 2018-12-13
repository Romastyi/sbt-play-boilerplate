package play.boilerplate.parser.backend.swagger

import play.boilerplate.parser.model.{EnumCreator, EnumDefinition, ObjectCreator, ObjectDefinition}

case class ParserContext(refCanBeLazy: Boolean, objectCreator: ObjectCreator, enumCreator: EnumCreator) {
  def withObjectCreator(creator: ObjectCreator): ParserContext = copy(objectCreator = creator)
  def withEnumCreator(creator: EnumCreator): ParserContext = copy(enumCreator = creator)
}

object ParserContext {

  def initial = ParserContext(
    refCanBeLazy = true,
    objectCreator = ObjectDefinition,
    enumCreator = EnumDefinition
  )

  def notLazy = ParserContext(
    refCanBeLazy = false,
    objectCreator = ObjectDefinition,
    enumCreator = EnumDefinition
  )

}