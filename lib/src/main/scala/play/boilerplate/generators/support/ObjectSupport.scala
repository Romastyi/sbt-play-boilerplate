package play.boilerplate.generators.support

import play.boilerplate.generators.{GeneratorContext, GeneratorUtils}
import play.boilerplate.parser.model._

trait ObjectSupport { this: DefinitionsSupport =>

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def getObjectSupport(obj: ObjectDefinition, context: DefinitionContext)
                      (implicit ctx: GeneratorContext): TypeSupport = {
    val className = obj.name.capitalize
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
    val objectClassName = fullClassName.split('.').last
    val objectClass = definitions.getClass(objectClassName)
    TypeSupport(
      tpe = objectClass,
      fullQualified = definitions.getClass(fullClassName),
      defs = if (context.withoutDefinition) {
        Nil
      } else {
        generateObjectDefs(objectClass, properties)(ctx.addCurrentPath(objectClass.nameString))
      }
    )
  }

  case class ObjectProperty(name: String, support: TypeSupport, noOptType: Type, isOpt: Boolean) {
    def param: ValDef = PARAM(name, support.tpe).empty
    def json : ObjectPropertyJson = ObjectPropertyJson(name, reads, writes)
    def reads : Enumerator = VALFROM(name) := PAREN(REF("JsPath") INFIX ("\\", LIT(name))) DOT (if (isOpt) "readNullable" else "read") APPLYTYPE noOptType
    def writes: Tree = LIT(name) INFIX ("->", REF("o") DOT name)
  }

  final case class ObjectJson(reads: Tree, writes: Tree)
  final case class ObjectPropertyJson(name: String, reads: Enumerator, writes: Tree)

  def generateObjectDefs(objectClass: Symbol, properties: Map[String, Definition])
                        (implicit ctx: GeneratorContext): Seq[TypeSupportDefs] = {

    val params = for ((name, prop) <- properties.toSeq) yield {
      val (noOptType, isOpt) = prop match {
        case OptionDefinition(_, base) => (getTypeSupport(base).tpe, true)
        case _ => (getTypeSupport(prop).tpe, false)
      }
      ObjectProperty(name, getTypeSupport(prop), noOptType, isOpt)
    }

    val objectDef = if (params.isEmpty) {
      CASEOBJECTDEF(objectClass).tree
    } else {
      CASECLASSDEF(objectClass).withParams(params.map(_.param)).tree
    }

    val ObjectJson(reads, writes) = generateObjectJson(objectClass, params)

    val objectDefs = TypeSupportDefs(
      symbol = objectClass,
      definition = objectDef,
      jsonReads  = reads,
      jsonWrites = writes,
      queryBindable = EmptyTree,
      pathBindable  = EmptyTree
    )

    val paramsDefs = params.flatMap(_.support.defs)
      .groupBy(_.symbol.nameString)
      .mapValues(_.head)
      .values.toSeq

    paramsDefs :+ objectDefs

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

  def generateObjectReads(modelName: String, modelType: Type, properties: Seq[ObjectPropertyJson])
                         (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val readsType  = definitions.getClass("Reads") TYPE_OF modelType

    val modelReads = VAL(s"${modelName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      if (caseObject) {
        ANONDEF(readsType) := LAMBDA(PARAM("json").tree) ==> REF("JsSuccess") APPLY REF(modelName)
      } else {
        FOR (properties.map(_.reads): _ *) YIELD REF(modelName) APPLY (properties.map(js => REF(js.name)): _ *)
      }
    }

    modelReads

  }

  def generateObjectWrites(modelName: String, modelType: Type, properties: Seq[ObjectPropertyJson])
                          (implicit ctx: GeneratorContext): Tree = {

    val writesType = definitions.getClass("Writes") TYPE_OF modelType

    val modelWrites = VAL(s"${modelName}Writes", writesType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(writesType) :=
        LAMBDA(PARAM("o").tree) ==> REF("Json") DOT "obj" APPLY (properties.map(_.writes): _ *)
    }

    modelWrites

  }

}
