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
    if (ctx.currentModel.nonEmpty && obj.inline) {
      composeName(ctx.settings.modelPackageName, pathClassName)
    } else if ((ctx.inService || ctx.inClient || ctx.inController || ctx.inRoutes) && obj.inline) {
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
    val support = if (context.canBeInterface && ctx.isCurrentInterface(obj) && !obj.inline) {
      generateInterface(fullClassName, params, obj.description)(newCtx)
    } else {
      generateObject(obj, fullClassName, params, Nil, context, withDefinition, obj.description)(newCtx)
    }
    support.copy(
      defs = support.defs.map { defs =>
        if (withDefinition) {
          defs
        } else {
          defs.copy(
            definition = EmptyTree,
            jsonReads  = EmptyTree,
            jsonWrites = EmptyTree
          )
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
    val parentsParams = interfaces.flatMap(_._2).map(_.copy(isOverride = true))
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
    val allParams: Seq[ObjectProperty] = parentsParams ++ ownParams
    val withDefinition = ctx.currentPath.isEmpty || complex.inline
    val support = generateObject(complex, fullClassName, allParams, parents, context, withDefinition, complex.description)(newCtx)
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

  private def generateInterface(fullClassName: String,
                                params: Seq[ObjectProperty],
                                description: Option[String])
                               (implicit ctx: GeneratorContext): TypeSupport = {
    val parts = fullClassName.split('.')
    val interfaceName = composeInterfaceName(parts.last)
    TypeSupport(
      tpe = RootClass.newClass(interfaceName),
      fullQualified = RootClass.newClass((parts.init :+ interfaceName).mkString(".")),
      defs = Seq(generateInterfaceDefs(
        interfaceName = RootClass.newClass(interfaceName),
        params = params,
        children = ctx.currentModel.map(_.children).getOrElse(Nil),
        description = description
      ))
    )
  }

  private def generateObject(definition: Definition,
                             fullClassName: String,
                             params: Seq[ObjectProperty],
                             parents: Seq[Symbol],
                             context: DefinitionContext,
                             withDefinition: Boolean,
                             description: Option[String])
                            (implicit ctx: GeneratorContext): TypeSupport = {
    val objectClassName = fullClassName.split('.').last
    val objectClass = RootClass.newClass(objectClassName)
    val interfaces = if (ctx.isCurrentInterface(definition)) {
      generateInterface(fullClassName, params, description).defs
    } else {
      Nil
    }
    val objectDefs = generateObjectDefs(objectClass, params, parents ++ interfaces.map(_.symbol), description)
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
        list +: (minItems.map(MinLength).toList ++ maxItems.map(MaxLength).toList)
      case RefDefinition(_, ref) =>
        collectPropertyConstraints(ref)
      case MapDefinition(_, _, additionalProperties) =>
        MapConstraint(collectPropertyConstraints(additionalProperties), getTypeSupport(additionalProperties).tpe) :: Nil
      case _: EmailDefinition =>
        Email :: Nil
      case p: WithMinMax[_] =>
        p.minimum.map(Minimum).toList ++ p.maximum.map(Maximum).toList
      case p: WithMinMaxLength =>
        p.minLength.map(MinLength).toList ++ p.maxLength.map(MaxLength).toList
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

  case class ObjectProperty(name: String,
                            support: TypeSupport,
                            noOptType: Type,
                            isOpt: Boolean,
                            isOverride: Boolean,
                            defaultValue: Option[Literal],
                            constraints: Seq[Constraint],
                            description: Option[String]) {
    val ident: String = stringToValidIdentifier(name, skipNotValidChars = true)
    def method: Tree = DEF(ident, support.tpe).empty.withDoc(description.getOrElse(""))
    def param : ValDef = {
      val p0 = if (isOverride) VAL(ident, support.tpe).withFlags(Flags.OVERRIDE) else PARAM(ident, support.tpe)
      defaultValue match {
        case Some(default) =>
          p0 := support.constructor(default)
        case None =>
          p0.empty
      }
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
    def docElement: DocElement = DocTag.Param(name, description.getOrElse(""))
  }

  case class ObjectJson(reads: Tree, writes: Tree)
  case class ObjectPropertyJson(ident: String, reads: Enumerator, writes: Tree)

  private def generateClassParams(properties: Map[String, Definition])
                                 (implicit ctx: GeneratorContext): Seq[ObjectProperty] = {
    for ((name, prop) <- properties.toSeq) yield {
      val support = getTypeSupport(prop)
      val (noOptType, isOpt) = prop match {
        case OptionDefinition(_, base) => (getTypeSupport(base).tpe, true)
        case _ => (support.tpe, false)
      }
      ObjectProperty(
        name = name,
        support = support,
        noOptType = noOptType,
        isOpt = isOpt,
        isOverride = false,
        defaultValue = getDefaultValue(prop),
        constraints = collectPropertyConstraints(prop),
        description = prop.description
      )
    }
  }

  private def generateInterfaceDefs(interfaceName: Symbol,
                                    params: Seq[ObjectProperty],
                                    children: Seq[Definition],
                                    description: Option[String])
                                   (implicit ctx: GeneratorContext): TypeSupportDefs = {

    val decl0 = TRAITDEF(interfaceName)
    val decl1 = if (ctx.modelsInOneFile) decl0.withFlags(Flags.SEALED) else decl0

    val definition = decl1 := BLOCK(
      params.map(_.method)
    )

    TypeSupportDefs(
      symbol = interfaceName,
      definition = definition.withDoc(description.getOrElse("")),
      jsonReads = generateInterfaceReads(interfaceName, children),
      jsonWrites = generateInterfaceWrites(interfaceName, children),
      queryBindable = EmptyTree,
      pathBindable = EmptyTree,
      queryParameter = EmptyTree,
      pathParameter = EmptyTree
    )

  }

  private def generateInterfaceReads(interfaceName: Symbol, children: Seq[Definition])
                                    (implicit ctx: GeneratorContext): Tree = {

    val readsType = RootClass.newClass("Reads") TYPE_OF interfaceName
    val ValidationErrorClass = RootClass.newClass("ValidationError")

    val cases = for (child <- children) yield {
      val support = getTypeSupport(child.baseDef)(ctx.setCurrentModel(None))
      val className = support.tpe.toString()
      val caseIn = CASE(LIT(className)) ==> (REF("JsPath") DOT "read" APPLYTYPE support.tpe)
      (className, caseIn)
    }

    val validationError = ValidationErrorClass APPLY INFIX_CHAIN(
      "+",
      LIT("""Field "__class" can be one of the specific values: """),
      REF("__classes") DOT "mkString" APPLY LIT(", ")
    )
    val validation = REF("Reads") DOT "filter" APPLYTYPE StringClass APPLY validationError APPLY REF("__classes")

    VAL(s"${interfaceName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := BLOCK(
      VAL("__classes", TYPE_SET(StringClass)) := ImmutableSetClass APPLY cases.map(_._1).map(LIT.apply),
      FOR(
        VALFROM("__class") := (PAREN(INFIX_CHAIN("""\""", REF("JsPath"), LIT("__class"))) DOT "read" APPLYTYPE StringClass APPLY validation),
        VALFROM("result") := REF("__class") MATCH cases.map(_._2)
      ) YIELD REF("result")
    )

  }

  private def generateInterfaceWrites(interfaceName: Symbol, children: Seq[Definition])
                                     (implicit ctx: GeneratorContext): Tree = {

    val jsValue = RootClass.newClass("JsValue")
    val jsObject = RootClass.newClass("JsObject")
    val writesType = RootClass.newClass("Writes") TYPE_OF interfaceName

    val cases = for ((child, idx) <- children.zipWithIndex) yield {
      val support = getTypeSupport(child.baseDef)(ctx.setCurrentModel(None))
      val className = support.tpe.toString()
      val writerIn = PARAM(s"w$idx", RootClass.newClass("Writes") TYPE_OF support.tpe).withFlags(Flags.IMPLICIT).empty
      val caseIn = CASE(ID("__class") withType support.tpe) ==> INFIX_CHAIN(
        "++",
        REF("Json") DOT "obj" APPLY INFIX_CHAIN("->", LIT("__class"), LIT(className)),
        REF("Json") DOT "toJson" APPLY REF("__class") APPLY REF(s"w$idx") DOT "as" APPLYTYPE jsObject
      )
      (writerIn, caseIn)
    }

    DEF(s"${interfaceName}Writes", writesType).withFlags(Flags.IMPLICIT).withParams(cases.map(_._1)) := {
      NEW(ANONDEF(writesType) := BLOCK(
        DEF("writes", jsValue).withFlags(Flags.OVERRIDE).withParams(PARAM("value", interfaceName).tree) := (
          REF("value") MATCH cases.map(_._2)
          )
      ))
    }

  }

  private def generateObjectDefs(objectClass: Symbol,
                                 properties: Seq[ObjectProperty],
                                 parents: Seq[Symbol],
                                 description: Option[String],
                                 withPropertiesDefs: Boolean = true)
                                (implicit ctx: GeneratorContext): Seq[TypeSupportDefs] = {

    val objectDef = if (properties.isEmpty) {
      CASEOBJECTDEF(objectClass).withParents(parents).tree
    } else {
      CASECLASSDEF(objectClass).withFlags(Flags.FINAL).withParams(properties.map(_.param)).withParents(parents).tree
    }

    val objectDefWithDoc = if (withPropertiesDefs) {
      objectDef.withDoc(
        description.toIndexedSeq,
        properties.map(_.docElement): _ *
      )
    } else objectDef

    val ObjectJson(reads, writes) = generateObjectJson(objectClass, properties)

    val objectDefs = TypeSupportDefs(
      symbol = objectClass,
      definition = objectDefWithDoc,
      jsonReads  = reads,
      jsonWrites = writes,
      queryBindable = generateObjectQueryBindable(objectClass, properties),
      pathBindable  = EmptyTree,
      queryParameter = generateObjectQueryParameter(objectClass, properties),
      pathParameter  = EmptyTree
    )

    val propertiesDefs = if (withPropertiesDefs) {
      properties.flatMap(_.support.defs)
        .groupBy(_.symbol.nameString)
        .mapValues(_.head)
        .values
        .toSeq
    } else Nil

    propertiesDefs :+ objectDefs

  }

  private def generateObjectJson(objectClass: Symbol, properties: Seq[ObjectProperty])
                                (implicit ctx: GeneratorContext): ObjectJson = {

    val caseObject = properties.isEmpty
    val modelName  = objectClass.nameString
    val modelType  = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)

    ObjectJson(
      reads  = generateObjectReads (modelName, modelType, properties.map(_.json)),
      writes = generateObjectWrites(modelName, modelType, properties)
    )

  }

  private def generateObjectReads(modelName: String, modelType: Type, properties: Seq[ObjectPropertyJson])
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

  private def generateObjectWrites(modelName: String, modelType: Type, properties: Seq[ObjectProperty])
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
            (idx + 1, tupleClass, tupleProps, generateObjectDefs(tupleClass, tupleProps, Nil, None, withPropertiesDefs = false))
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

  private def generateObjectQueryBindable(objectClass: Symbol, properties: Seq[ObjectProperty])
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

  private def generateObjectQueryParameter(objectClass: Symbol, properties: Seq[ObjectProperty])
                                          (implicit ctx: GeneratorContext): Tree = {

    val caseObject = properties.isEmpty
    val modelName  = objectClass.nameString
    val modelType  = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)
    val queryParameterType = RootClass.newClass("QueryParameter") TYPE_OF modelType


    if (caseObject) {
      VAL(s"${modelName}QueryParameter", queryParameterType).withFlags(Flags.IMPLICIT, Flags.LAZY) := {
        REF("QueryParameter") APPLYTYPE StringClass DOT "transform" APPLY {
          LAMBDA(PARAM(WILDCARD).tree) ==> LIT("")
        }
      }
    } else {
      val propertyRenders = for (property <- properties) yield {
        REF("QueryParameter") DOT "render" APPLYTYPE property.support.fullQualified APPLY(
          INFIX_CHAIN("+", REF("key"), LIT("." + property.name)),
          REF("value") DOT property.name
        )
      }
      OBJECTDEF(s"${modelName}QueryParameter")
        .withParents(queryParameterType)
        .withFlags(Flags.IMPLICIT) :=
        BLOCK {
          DEF("render", StringClass)
            .withParams(PARAM("key", StringClass).tree, PARAM("value", modelType).tree)
            .withFlags(Flags.OVERRIDE) :=
            BLOCK {
              REF("QueryParameter") DOT "concatAll" APPLY SEQ(propertyRenders)
            }
        }
    }

  }

}
