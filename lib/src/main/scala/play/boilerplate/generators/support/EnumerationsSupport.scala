package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait EnumerationsSupport {

  import treehugger.forest._

  def getEnumSupport(enum: EnumDefinition, context: DefinitionContext)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    val className = enum.name.capitalize
    val pathClassName = (ctx.currentPath.map(_.capitalize) :+ className).mkString("")
    val fullClassName = if (ctx.inModel && context.isModel) {
      Seq(ctx.modelPackageName, pathClassName).mkString(".")
    } else if (ctx.inService && context.isParameter) {
      Seq(ctx.servicePackageName, ctx.serviceClassName, pathClassName).mkString(".")
    } else {
      className
    }
    val haveDefinitions = (ctx.inModel && context.isModel) || (ctx.inService && context.isParameter)
    val support = ctx.enumGenerator.getEnumerationSupport(fullClassName, enum.items)
    support.copy(
      defs = support.defs.map { defs =>
        if (haveDefinitions) {
          defs
        } else {
          defs.copy(definition = EmptyTree)
        }
      }
    )
  }

}
