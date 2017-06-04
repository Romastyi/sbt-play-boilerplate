package play.boilerplate.generators.support

case class DefinitionContext(isModel: Boolean, isParameter: Boolean, isProperty: Boolean)

object DefinitionContext {
  def empty     = DefinitionContext(isModel = false, isParameter = false, isProperty = false)
  def model     = DefinitionContext(isModel = true , isParameter = false, isProperty = false)
  def parameter = DefinitionContext(isModel = false, isParameter = true , isProperty = false)
  def property  = DefinitionContext(isModel = false, isParameter = false, isProperty = true )
}
