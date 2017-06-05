package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait ObjectSupport { this: DefinitionsSupport =>

  import treehugger.forest._
  import definitions._
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

  case class ObjectProperty(name: String, support: TypeSupport, isOpt: Boolean) {
    def param: ValDef = PARAM(name, support.tpe).empty
    def json : ObjectJson = ObjectJson(reads, writes)
    def reads : Tree = PAREN(REF("json") INFIX ("\\", LIT(name))) DOT (if (isOpt) "asOpt" else "as") APPLYTYPE support.tpe
    def writes: Tree = LIT(name) INFIX ("->", (REF("Json") DOT "toJson")(REF("o") DOT name))
  }

  case class ObjectJson(reads: Tree, writes: Tree)

  def generateObjectDefs(objectClass: Symbol, properties: Map[String, Definition])
                        (implicit ctx: GeneratorContext): Seq[TypeSupportDefs] = {

    val params = for ((name, prop) <- properties.toSeq) yield {
      val isOpt = prop match {
        case _: OptionDefinition => true
        case _ => false
      }
      ObjectProperty(name, getTypeSupport(prop), isOpt)
    }

    val objectDef = if (params.isEmpty) {
      CASEOBJECTDEF(objectClass).tree
    } else {
      CASECLASSDEF(objectClass).withParams(params.map(_.param)).tree
    }

    val ObjectJson(reads, writes) = generateObjectJson(objectClass, params)
    val paramsReads  = params.flatMap(_.support.defs.map(_.jsonReads ))
    val paramsWrites = params.flatMap(_.support.defs.map(_.jsonWrites))

    val objectDefs = TypeSupportDefs(
      symbol = objectClass,
      definition = objectDef,
      jsonReads  = reads,
      jsonWrites = writes,
      jsonObject = TypeSupport.generateJsonObject(objectClass, paramsReads :+ reads, paramsWrites :+ writes),
      queryBindable = EmptyTree,
      pathBindable  = EmptyTree
    )

    params.flatMap(_.support.defs) :+ objectDefs

  }

  def generateObjectJson(objectClass: Symbol, properties: Seq[ObjectProperty])
                        (implicit ctx: GeneratorContext): ObjectJson = {

    val caseObject = properties.isEmpty
    val modelName  = objectClass.nameString
    val modelType  = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)

    ObjectJson(
      reads  = generateObjectReads (modelName, modelType, properties.map(_.json)),
      writes = generateObjectWrites(modelName, modelType, properties.map(_.json))
    )

  }

  def generateObjectReads(modelName: String, modelType: Type, properties: Seq[ObjectJson])
                         (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val readsType  = definitions.getClass("Reads") TYPE_OF modelType

    val modelReads = VAL(s"${modelName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(readsType) :=
        LAMBDA(PARAM("json").tree) ==>
          REF("JsSuccess") APPLY {
          if (caseObject) {
            REF(modelName)
          } else {
            REF(modelName) APPLY properties.map(_.reads)
          }
        }
    }

    modelReads

  }

  def generateObjectWrites(modelName: String, modelType: Type, properties: Seq[ObjectJson])
                          (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val writesType = definitions.getClass("Writes") TYPE_OF modelType

    val modelWrites = VAL(s"${modelName}Writes", writesType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(writesType) :=
        LAMBDA(PARAM("o").tree) ==>
          REF("JsObject") APPLY {
          if (caseObject) {
            SeqClass APPLY Seq.empty
          } else {
            SeqClass APPLY properties.map(_.writes) DOT "filter" APPLY (REF("_") DOT "_2" INFIX ("!=", REF("JsNull")))
          }
        }
    }

    modelWrites

  }

}
