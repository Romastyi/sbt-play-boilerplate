package play.boilerplate.generators.support

case class DefinitionContext(canBeOption: Boolean, canBeInterface: Boolean)

object DefinitionContext {
  def default = DefinitionContext(canBeOption = true, canBeInterface = false)
}
