package play.boilerplate.generators.support

import play.boilerplate.generators.{GeneratorContext, GeneratorUtils}
import play.boilerplate.parser.model._

trait ObjectSupport { this: DefinitionsSupport =>

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._
  import definitions._

  val MaxJsonArity = 21

  def composeClassName(objName: String): String = objName.capitalize

  def composeInterfaceName(objName: String): String = "I" + composeClassName(objName)

  def composeFullClassName(objName: String, obj: ComplexDefinition)(implicit ctx: GeneratorContext): String = {
    val className = objName.capitalize
    val pathClassName = (ctx.currentPath.map(_.capitalize) :+ className).mkString("")
    if (ctx.inModel && obj.inline) {
      composeName(ctx.settings.modelPackageName, pathClassName)
    } else if ((ctx.inService || ctx.inClient) && obj.inline) {
      composeName(ctx.settings.servicePackageName, ctx.settings.serviceClassName, pathClassName)
    } else {
      val packageName = if (ctx.isModel) {
        ctx.settings.modelPackageName
      } else {
        ctx.settings.basePackageName
      }
      composeName(packageName, className)
    }
  }

  def getObjectSupport(obj: ObjectDefinition, context: DefinitionContext)
                      (implicit ctx: GeneratorContext): TypeSupport = {
    val fullClassName = composeFullClassName(obj.name, obj)
    val newCtx = ctx.addCurrentPath(composeClassName(obj.name))
    val params = generateClassParams(obj.properties)(newCtx)
    val withDefinition = ctx.currentPath.isEmpty || obj.inline
    val support = generateObject(fullClassName, params, Nil, context, withDefinition)(newCtx)
    support.copy(
      defs = support.defs.map { defs =>
        if (withDefinition) {
          defs
        } else {
          defs.copy(definition = EmptyTree)
        }
      }
    )
  }

  def getComplexObjectSupport(complex: ComplexObjectDefinition, context: DefinitionContext)
                             (implicit ctx: GeneratorContext): TypeSupport = {
    val fullClassName = composeFullClassName(complex.name, complex)
    val interfaces = complex.interfaces.map(
      _.baseDef match {
        case obj: ObjectDefinition =>
          val symbol = RootClass.newClass(composeInterfaceName(obj.name))
          val params = generateClassParams(obj.properties)(ctx.addCurrentPath(composeClassName(obj.name)))
          (symbol, params)
        case other =>
          throw new RuntimeException(s"Complex object could not inherits any definition except object ($other).")
      }
    )
    val parents = interfaces.map(_._1)
    val parentsParams = interfaces.flatMap(_._2)
    val ownProperties = complex.inlines.map(
      _.baseDef match {
        case obj: ObjectDefinition =>
          obj.properties
        case other =>
          throw new RuntimeException(s"Complex object could not inlines any definition except object ($other).")
      }
    ).reduceLeftOption(_ ++ _).getOrElse(Map.empty)
    val newCtx = ctx.addCurrentPath(composeClassName(complex.name))
    val ownParams = generateClassParams(ownProperties)(newCtx)
    val withDefinition = ctx.currentPath.isEmpty || complex.inline
    val support = generateObject(fullClassName, parentsParams ++ ownParams, parents, context, withDefinition)(newCtx)
    support.copy(
      defs = support.defs.map { defs =>
        if (withDefinition) {
          defs
        } else {
          defs.copy(definition = EmptyTree)
        }
      }
    )
  }

  def generateObject(fullClassName: String,
                     params: Seq[ObjectProperty],
                     parents: Seq[Symbol],
                     context: DefinitionContext,
                     withDefinition: Boolean)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    val objectClassName = fullClassName.split('.').last
    val objectClass = RootClass.newClass(objectClassName)
    val interfaces = if (ctx.needInterface) {
      Seq(generateInterfaceDefs(RootClass.newClass(composeInterfaceName(objectClassName)), params))
    } else {
      Nil
    }
    val objectDefs = generateObjectDefs(objectClass, params, parents ++ interfaces.map(_.symbol))
    TypeSupport(
      tpe = objectClass,
      fullQualified = RootClass.newClass(fullClassName),
      defs = if (withDefinition) {
        interfaces ++ objectDefs
      } else {
        Nil
      }
    )
  }

  sealed trait Constraint
  case class ListConstraint(constraints: Seq[Constraint], tpe: Type) extends Constraint
  case class MapConstraint(constraints: Seq[Constraint], tpe: Type) extends Constraint
  case class Maximum(value: Any) extends Constraint
  case class Minimum(value: Any) extends Constraint
  case class MaxLength(length: Int) extends Constraint
  case class MinLength(length: Int) extends Constraint
  case class Pattern(pattern: String) extends Constraint
  case object Email extends Constraint

  def collectPropertyConstraints(property: Definition)(implicit ctx: GeneratorContext): Seq[Constraint] = {
    property match {
      case OptionDefinition(_, base) =>
        collectPropertyConstraints(base)
      case ArrayDefinition(_, _, items, _, minItems, maxItems, _) =>
        val list = ListConstraint(collectPropertyConstraints(items), getTypeSupport(items).tpe)
        list +: (minItems.map(MaxLength).toList ++ maxItems.map(MinLength).toList)
      case RefDefinition(_, ref) =>
        collectPropertyConstraints(ref)
      case MapDefinition(_, _, additionalProperties) =>
        MapConstraint(collectPropertyConstraints(additionalProperties), getTypeSupport(additionalProperties).tpe) :: Nil
      case _: EmailDefinition =>
        Email :: Nil
      case p: WithMinMax[_] =>
        p.maximum.map(Maximum).toList ++ p.minimum.map(Minimum).toList
      case p: WithMinMaxLength =>
        p.maxLength.map(MaxLength).toList ++ p.minLength.map(MinLength).toList
      case p: WithPattern =>
        p.pattern.map(Pattern).toList
      case _ =>
        Nil
    }
  }

  def getReadsConstraint(constraint: Constraint, noOptType: Type): Tree = {
    constraint match {
      case ListConstraint(constraints, tpe) if constraints.nonEmpty =>
        REF("Reads") DOT "list" APPLYTYPE tpe APPLY INFIX_CHAIN("keepAnd", constraints.map(getReadsConstraint(_, tpe)))
      case MapConstraint(constraints, tpe) if constraints.nonEmpty =>
        REF("Reads") DOT "map" APPLYTYPE tpe APPLY INFIX_CHAIN("keepAnd", constraints.map(getReadsConstraint(_, tpe)))
      case Maximum(value) =>
        REF("Reads") DOT "max" APPLYTYPE noOptType APPLY LIT(value)
      case Minimum(value) =>
        REF("Reads") DOT "min" APPLYTYPE noOptType APPLY LIT(value)
      case MaxLength(length) =>
        REF("Reads") DOT "maxLength" APPLYTYPE noOptType APPLY LIT(length)
      case MinLength(length) =>
        REF("Reads") DOT "minLength" APPLYTYPE noOptType APPLY LIT(length)
      case Pattern(pattern) =>
        REF("Reads") DOT "pattern"   APPLYTYPE noOptType APPLY (LIT(pattern) DOT "r")
      case Email =>
        REF("Reads") DOT "email"
      case _ =>
        EmptyTree
    }
  }

  case class ObjectProperty(name: String, support: TypeSupport, noOptType: Type, isOpt: Boolean, defaultValue: Option[Literal], constraints: Seq[Constraint]) {
    val ident: String = stringToValidIdentifier(name, skipNotValidChars = true)
    def method: DefDef = DEF(ident, support.tpe).empty
    def param : ValDef = defaultValue match {
      case Some(default) =>
        PARAM(ident, support.tpe) := support.constructor(default)
      case None =>
        PARAM(ident, support.tpe).empty
    }
    def json  : ObjectPropertyJson = ObjectPropertyJson(ident, reads, writes)
    def reads : Enumerator = {
      val readsConstraints = filterNonEmptyTree(constraints.map(getReadsConstraint(_, noOptType)))
      val readsDef = PAREN(REF("JsPath") INFIX ("\\", LIT(name))) DOT (if (isOpt) "readNullable" else "read") APPLYTYPE noOptType
      VALFROM(ident) := {
        if (readsConstraints.isEmpty) {
          readsDef
        } else {
          readsDef APPLY INFIX_CHAIN("keepAnd", readsConstraints)
        }
      }
    }
    def writes: Tree = {
      PAREN(REF("JsPath") INFIX ("\\", LIT(name))) DOT (if (isOpt) "writeNullable" else "write") APPLYTYPE noOptType
    }
  }

  case class ObjectJson(reads: Tree, writes: Tree)
  case class ObjectPropertyJson(ident: String, reads: Enumerator, writes: Tree)

  def generateClassParams(properties: Map[String, Definition])
                         (implicit ctx: GeneratorContext): Seq[ObjectProperty] = {
    for ((name, prop) <- properties.toSeq) yield {
      val support = getTypeSupport(prop)
      val (noOptType, isOpt) = prop match {
        case OptionDefinition(_, base) => (getTypeSupport(base).tpe, true)
        case _ => (support.tpe, false)
      }
      ObjectProperty(name, support, noOptType, isOpt, getDefaultValue(prop), collectPropertyConstraints(prop))
    }
  }

  def generateInterfaceDefs(interfaceName: Symbol, params: Seq[ObjectProperty])
                           (implicit ctx: GeneratorContext): TypeSupportDefs = {

    val definition = TRAITDEF(interfaceName)/*.withFlags(Flags.SEALED)*/ := BLOCK(
      params.map(_.method)
    )

    TypeSupportDefs(
      symbol = interfaceName,
      definition = definition,
      jsonReads = EmptyTree,
      jsonWrites = EmptyTree,
      queryBindable = EmptyTree,
      pathBindable = EmptyTree
    )

  }

  def generateObjectDefs(objectClass: Symbol, params: Seq[ObjectProperty], parents: Seq[Symbol])
                        (implicit ctx: GeneratorContext): Seq[TypeSupportDefs] = {

    val objectDef = if (params.isEmpty) {
      CASEOBJECTDEF(objectClass).withParents(parents).tree
    } else {
      CASECLASSDEF(objectClass).withParams(params.map(_.param)).withParents(parents).tree
    }

    val ObjectJson(reads, writes) = generateObjectJson(objectClass, params)

    val objectDefs = TypeSupportDefs(
      symbol = objectClass,
      definition = objectDef,
      jsonReads  = reads,
      jsonWrites = writes,
      queryBindable = generateObjectQueryBindable(objectClass, params),
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
      writes = generateObjectWrites(modelName, modelType, properties)
    )

  }

  def generateObjectReads(modelName: String, modelType: Type, properties: Seq[ObjectPropertyJson])
                         (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val readsType  = RootClass.newClass("Reads") TYPE_OF modelType

    val modelReads = VAL(s"${modelName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      if (caseObject) {
        REF("Reads") DOT "pure" APPLY REF(modelName)
      } else {
        FOR (properties.map(_.reads): _ *) YIELD REF(modelName) APPLY (properties.map(js => REF(js.ident)): _ *)
      }
    }

    modelReads

  }

  def generateObjectWrites(modelName: String, modelType: Type, properties: Seq[ObjectProperty])
                          (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val writesType = RootClass.newClass("Writes") TYPE_OF modelType

    val modelWrites = VAL(s"${modelName}Writes", writesType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      if (caseObject) {
        ANONDEF(writesType) := LAMBDA(PARAM(WILDCARD).tree) ==> REF("Json") DOT "obj" APPLY ()
      } else {
        if (properties.size > MaxJsonArity) {
          val tupleDefs = for ((tupleProps, idx) <- properties.grouped(MaxJsonArity).toIndexedSeq.zipWithIndex) yield {
            val tupleClass = RootClass.newClass(s"${modelName}Tuple${idx + 1}")
            (idx + 1, tupleClass, tupleProps, generateObjectDefs(tupleClass, tupleProps, Nil))
          }
          val tupleWrites = tupleDefs.flatMap { case (_, _, _, definitions) =>
            definitions.flatMap(i => Seq(i.definition, i.jsonWrites))
          }
          val unapplyTuple = TYPE_TUPLE(tupleDefs.map(_._2))
          val unapplyDef = DEF("unapplyClass", unapplyTuple)
            .withParams(PARAM("value", modelType).empty) :=
            BLOCK {
              val values = for ((_, tupleClass, tupleProps, _) <- tupleDefs) yield {
                tupleClass APPLY tupleProps.map { prop =>
                  REF(prop.ident) := (REF("value") DOT prop.ident)
                }
              }
              TUPLE(values)
            }
          val propertiesChain = INFIX_CHAIN("and", tupleDefs.map {
            case (_, tupleClass, _, _) => REF("JsPath") DOT "write" APPLYTYPE tupleClass
          })
          val jsonWrites = PAREN(propertiesChain) APPLY (REF("unapplyClass") APPLY WILDCARD)
          BLOCK(tupleWrites :+ unapplyDef :+ jsonWrites)
        } else {
          val propertiesChain = INFIX_CHAIN("and", properties.map(_.json.writes))
          val unliftUnapply = REF("unlift") APPLY (REF(modelName) DOT "unapply")
          if (properties.length > 1) {
            PAREN(propertiesChain) APPLY unliftUnapply
          } else {
            propertiesChain DOT "contramap" APPLY unliftUnapply
          }
        }
      }
    }

    modelWrites

  }

  def generateObjectQueryBindable(objectClass: Symbol, properties: Seq[ObjectProperty])
                                 (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val modelName  = objectClass.nameString
    val modelType  = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)
    val queryBindableType = RootClass.newClass("QueryStringBindable") TYPE_OF modelType

    val ExceptionClass = RootClass.newClass("Exception")

    if (caseObject) {
      val bindable = REF("QueryStringBindable") DOT "Parsing" APPLYTYPE modelType APPLY(
        LAMBDA(PARAM(WILDCARD).tree) ==> REF(modelName),
        LAMBDA(PARAM(WILDCARD).tree) ==> LIT(""),
        LAMBDA(PARAM("key", StringClass).tree, PARAM("e", ExceptionClass).tree) ==>
          LIT(s"Cannot parse parameter %s as ${modelType.toString()}: %s") DOT "format"
          APPLY(REF("key"), REF("e") DOT "getMessage")
      )
      OBJECTDEF(s"${modelName}QueryBindable").withParents(bindable).withFlags(Flags.IMPLICIT).tree
    } else {
      VAL(s"${modelName}QueryBindable", queryBindableType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
        RootClass.newClass("play.boilerplate.api.common.Binders") DOT "queryStringStrict"
      }
    }

  }

}
