package play.boilerplate.generators

import play.boilerplate.generators.support.{TypeSupport, TypeSupportDefs}

trait EnumerationGenerator {
  def getEnumerationSupport(fullClassName: String, items: Iterable[String]): TypeSupport
}

object VanillaEnumerations extends EnumerationGenerator {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private lazy val EnumerationClass = definitions.getClass("Enumeration")
  private lazy val ExceptionClass = definitions.getClass("Exception")

  def enumerationValueType(enumClass: Symbol): Type = {
    TYPE_REF(enumClass DOT "Value")
  }

  def generateEnumeration(enumClass: Symbol, items: Iterable[String]): ImplDef = {

    OBJECTDEF(enumClass) withParents EnumerationClass := BLOCK {
      items.map { item =>
        VAL(item.capitalize) := REF("Value") APPLY LIT(item)
      }
    }

  }

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
      defs = generateEnumDefs(enumClass, items)
    )
  }

}
