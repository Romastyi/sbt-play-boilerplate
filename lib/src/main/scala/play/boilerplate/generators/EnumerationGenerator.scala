package play.boilerplate.generators

import play.boilerplate.generators.support.{TypeSupport, TypeSupportDefs}
import treehugger.forest._
import definitions._
import treehuggerDSL._

trait EnumerationGenerator {
  def getEnumerationSupport(fullClassName: String, items: Iterable[String]): TypeSupport
}

sealed trait CommonEnumerations extends EnumerationGenerator {

  def enumerationValueType(enumClass: Symbol): Type = {
    TYPE_REF(enumClass DOT "Value")
  }

  def generateEnumeration(enumClass: Symbol, items: Iterable[String]): ImplDef

  def generateEnumReads(enumClass: Symbol): ValDef = {

    val enumValueType = enumerationValueType(enumClass)
    val readsType = definitions.getClass("Reads") TYPE_OF enumValueType

    VAL(s"${enumClass.nameString}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(readsType) := BLOCK (
        CASE(REF("JsString") UNAPPLY ID("name")) ==> {
          TRY {
            REF("JsSuccess") APPLY (enumClass DOT "withName" APPLY REF("name"))
          } CATCH {
            CASE(WILDCARD withType TYPE_REF("NoSuchElementException")) ==> {
              REF("JsError") APPLY INFIX_CHAIN("+",
                LIT("Enumeration expected of type: '"),
                enumClass DOT "getClass",
                LIT("', but it does not appear to contain the value: '"),
                REF("name"),
                LIT("'.")
              )
            }
          } ENDTRY
        },
        CASE(WILDCARD) ==> {
          REF("JsError") APPLY INFIX_CHAIN("+",
            LIT("Enumeration expected of type: '"),
            enumClass DOT "getClass",
            LIT("'.")
          )
        }
      )
    }

  }

  def generateEnumWrites(enumClass: Symbol): ValDef = {

    val enumValueType = enumerationValueType(enumClass)
    val writesType = definitions.getClass("Writes") TYPE_OF enumValueType

    VAL(s"${enumClass.nameString}Writes", writesType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(writesType) :=
        LAMBDA(PARAM("value").tree) ==>
          REF("JsString") APPLY (REF("value") DOT "toString")
    }

  }

  def generateEnumQueryBindable(enumClass: Symbol): Tree = {
    generateEnumBindable(enumClass, "QueryStringBindable", "Query")
  }

  def generateEnumPathBindable(enumClass: Symbol): Tree = {
    generateEnumBindable(enumClass, "PathBindable", "Path")
  }

  def generateEnumBindable(enumClass: Symbol, baseClassName: String, classSuffix: String): Tree = {

    val enumValue = enumerationValueType(enumClass)
    val ExceptionClass = definitions.getClass("Exception")

    val bindable = (TYPE_REF(baseClassName) DOT "Parsing") APPLYTYPE enumValue APPLY(
      enumClass DOT "withName",
      WILDCARD DOT "toString",
      LAMBDA(PARAM("key", StringClass).tree, PARAM("e", ExceptionClass).tree) ==>
        LIT(s"Cannot parse parameter %s as ${enumValue.toString()}: %s") DOT "format"
        APPLY(REF("key"), REF("e") DOT "getMessage")
    )

    OBJECTDEF(enumClass.nameString + classSuffix).withParents(bindable).withFlags(Flags.IMPLICIT).tree

  }

  def generateEnumDefs(enumClass: Symbol, items: Iterable[String]): Seq[TypeSupportDefs] = {
    TypeSupportDefs(
      symbol     = enumClass,
      definition = generateEnumeration(enumClass, items),
      jsonReads  = generateEnumReads(enumClass),
      jsonWrites = generateEnumWrites(enumClass),
      queryBindable = generateEnumQueryBindable(enumClass),
      pathBindable  = generateEnumPathBindable(enumClass)
    ) :: Nil
  }

  override def getEnumerationSupport(fullClassName: String, items: Iterable[String]): TypeSupport = {
    val enumClassName = fullClassName.split('.').last
    val enumClass = definitions.getClass(enumClassName)
    TypeSupport(
      tpe = enumerationValueType(enumClass),
      fullQualified = enumerationValueType(definitions.getClass(fullClassName)),
      defs = generateEnumDefs(enumClass, items),
      constructor = l => definitions.getClass(fullClassName) DOT "withName" APPLY l
    )
  }

}

object VanillaEnumerations extends CommonEnumerations {

  override def generateEnumeration(enumClass: Symbol, items: Iterable[String]): ImplDef = {
    val EnumerationClass = definitions.getClass("Enumeration")
    OBJECTDEF(enumClass) withParents EnumerationClass := BLOCK {
      items.map { item =>
        VAL(item.capitalize) := REF("Value") APPLY LIT(item)
      }
    }
  }

}

object SealedTraitEnumerations extends CommonEnumerations {

  override def generateEnumeration(enumClass: Symbol, items: Iterable[String]): ImplDef = {
    val NoSuchElementExceptionClass = definitions.getClass("NoSuchElementException")
    val valueTpe = TYPE_REF("Value")
    val traitDef: Tree = TRAITDEF("Value").withFlags(Flags.SEALED) withParents orderedType(valueTpe) := BLOCK(
      DEF("id", IntClass).empty,
      DEF("name", StringClass).empty,
      DEF("toString", StringClass).withFlags(Flags.OVERRIDE) := REF("name"),
      DEF("compare", IntClass).withFlags(Flags.OVERRIDE).withParams(PARAM("that", valueTpe).tree) := BLOCK {
        IF((THIS DOT "id") INFIX("<", REF("that") DOT "id")) THEN LIT(-1) ELSE {
          IF((THIS DOT "id") INFIX("==", REF("that") DOT "id")) THEN LIT(0) ELSE LIT(1)
        }
      }
    )
    val casesDef: Seq[Tree] = items.toIndexedSeq.zipWithIndex.map { case (item, idx) =>
      CASEOBJECTDEF(item.capitalize) withParents valueTpe := BLOCK(
        VAL("id", IntClass).withFlags(Flags.OVERRIDE) := LIT(idx),
        VAL("name", StringClass).withFlags(Flags.OVERRIDE) := LIT(item)
      )
    }
    val allValues: Tree = VAL("values", ImmutableSetClass TYPE_OF valueTpe) := {
      ImmutableSetClass.APPLY(items.toIndexedSeq.map(item => REF(item.capitalize)))
    }
    val withName: Tree = DEF("withName", valueTpe).withParams(PARAM("s", StringClass).tree) := {
      REF("values") DOT "find" APPLY (WILDCARD DOT "toString" INFIX("==", REF("s"))) DOT "getOrElse" APPLY {
        THROW(NEW(
          NoSuchElementExceptionClass,
          INTERP(StringContext_s, LIT("No value found for '"), REF("s"), LIT("' in enumeration '"), THIS DOT "getClass", LIT("'."))
        ))
      }
    }
    OBJECTDEF(enumClass) := BLOCK(
      traitDef +: casesDef :+ allValues :+ withName
    )
  }

}