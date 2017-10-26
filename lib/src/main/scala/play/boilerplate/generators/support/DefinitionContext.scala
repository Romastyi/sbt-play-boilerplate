package play.boilerplate.generators.support

case class DefinitionContext(withoutDefinition: Boolean, isInline: Boolean, canBeOption: Boolean)

object DefinitionContext {
  def inline            = DefinitionContext(withoutDefinition = false, isInline = true, canBeOption = true)
  def withoutDefinition = DefinitionContext(withoutDefinition = true, isInline = false, canBeOption = true)
}
