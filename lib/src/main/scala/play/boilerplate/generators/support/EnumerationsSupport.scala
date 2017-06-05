package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait EnumerationsSupport {

  import treehugger.forest._

  def getEnumSupport(enum: EnumDefinition, context: DefinitionContext)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    val className = enum.name.capitalize
    val pathClassName = (ctx.currentPath.map(_.capitalize) :+ className).mkString("")
    val fullClassName = if (ctx.inModel && context.isInline) {
      Seq(ctx.modelPackageName, pathClassName).mkString(".")
    } else if (ctx.inService && context.isInline) {
      Seq(ctx.servicePackageName, ctx.serviceClassName, pathClassName).mkString(".")
    } else {
      className
    }
    val support = ctx.enumGenerator.getEnumerationSupport(fullClassName, enum.items)
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
