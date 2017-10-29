package play.boilerplate.generators.support

import play.boilerplate.generators.{GeneratorContext, GeneratorUtils}
import play.boilerplate.parser.model._

trait EnumerationsSupport {

  import GeneratorUtils._
  import treehugger.forest._

  def getEnumSupport(enum: EnumDefinition, context: DefinitionContext)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    val className = enum.name.capitalize
    val pathClassName = (ctx.currentPath.map(_.capitalize) :+ className).mkString("")
    val fullClassName = if (ctx.inModel && context.isInline) {
      composeName(ctx.settings.modelPackageName, pathClassName)
    } else if ((ctx.inService || ctx.inClient) && context.isInline) {
      composeName(ctx.settings.servicePackageName, ctx.settings.serviceClassName, pathClassName)
    } else {
      val packageName = if (ctx.isModel) {
        ctx.settings.modelPackageName
      } else {
        ctx.settings.basePackageName
      }
      composeName(packageName, className)
    }
    val support = ctx.settings.enumGenerator.getEnumerationSupport(fullClassName, enum.items)
    support.copy(
      defs = support.defs.map { defs =>
        if (context.withoutDefinition) {
          defs.copy(definition = EmptyTree)
        } else {
          defs
        }
      }
    )
  }

}
