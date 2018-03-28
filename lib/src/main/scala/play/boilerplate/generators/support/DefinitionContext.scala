package play.boilerplate.generators.support

case class DefinitionContext(canBeOption: Boolean)

object DefinitionContext {
  def default = DefinitionContext(canBeOption = true)
}
