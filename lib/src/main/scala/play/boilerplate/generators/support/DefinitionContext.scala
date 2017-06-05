package play.boilerplate.generators.support

case class DefinitionContext(withoutDefinition: Boolean, isInline: Boolean)

object DefinitionContext {
  def inline            = DefinitionContext(withoutDefinition = false, isInline = true)
  def withoutDefinition = DefinitionContext(withoutDefinition = true, isInline = false)
}
