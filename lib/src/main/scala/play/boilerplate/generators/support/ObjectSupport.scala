package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait ObjectSupport { this: DefinitionsSupport =>

  import treehugger.forest._
  import treehuggerDSL._

  def getObjectSupport(obj: ObjectDefinition, context: DefinitionContext)
                      (implicit ctx: GeneratorContext): TypeSupport = {
    val className = obj.name.capitalize
    val pathClassName = (ctx.currentPath.map(_.capitalize) :+ className).mkString("")
    val fullClassName = if (ctx.inModel && context.isInline) {
      Seq(ctx.modelPackageName, pathClassName).mkString(".")
    } else if (ctx.inService && context.isInline) {
      Seq(ctx.servicePackageName, ctx.serviceClassName, pathClassName).mkString(".")
    } else {
      className
    }
    val support = generateObject(fullClassName, obj.properties, context)
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

  case class ObjectProperty(name: String, support: TypeSupport) {
    def param: ValDef = PARAM(name, support.tpe).empty
  }

  def generateObjectDefs(objectClass: Symbol, properties: Map[String, Definition])
                        (implicit ctx: GeneratorContext): Seq[TypeSupportDefs] = {

    val params = for ((name, prop) <- properties) yield {
      name -> ObjectProperty(name, getTypeSupport(prop))
    }

    val objectDef = if (params.isEmpty) {
      CASEOBJECTDEF(objectClass).tree
    } else {
      CASECLASSDEF(objectClass).withParams(params.values.map(_.param)).tree
    }

    val objectDefs = TypeSupportDefs(
      definition = objectDef,
      jsonReads = EmptyTree,
      jsonWrites = EmptyTree,
      queryBindable = EmptyTree,
      pathBindable = EmptyTree
    )

    params.values.flatMap(_.support.defs).toIndexedSeq :+ objectDefs

  }

  def generateObject(fullClassName: String, properties: Map[String, Definition], context: DefinitionContext)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    val objectClass = definitions.getClass(fullClassName)
    TypeSupport(
      tpe = definitions.getClass(objectClass.nameString),
      fullQualified = definitions.getClass(objectClass.fullName('.')),
      defs = if (context.withoutDefinition) {
        Nil
      } else {
        generateObjectDefs(objectClass, properties)(ctx.addCurrentPath(objectClass.nameString))
      }
    )
  }

}
