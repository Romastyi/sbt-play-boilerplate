package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait CustomTypeSupport {

  import CustomTypeSupport._

  def getComplexTypeSupport: GeneratorContext => ComplexTypeSupport

  def getSimpleTypeSupport: GeneratorContext => SimpleTypeSupport

  def ++(other: CustomTypeSupport): CustomTypeSupport = {
    val sts0: (GeneratorContext) => SimpleTypeSupport = { ctx =>
      this.getSimpleTypeSupport(ctx) orElse other.getSimpleTypeSupport(ctx)
    }
    val cts0: (GeneratorContext) => ComplexTypeSupport = { ctx =>
      this.getComplexTypeSupport(ctx) orElse other.getComplexTypeSupport(ctx)
    }
    new CustomTypeSupport {
      override def getSimpleTypeSupport: (GeneratorContext) => SimpleTypeSupport = sts0
      override def getComplexTypeSupport: (GeneratorContext) => ComplexTypeSupport = cts0
    }
  }

}

object CustomTypeSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  type ComplexTypeSupport = PartialFunction[(ComplexDefinition, DefinitionContext), TypeSupport]
  type SimpleTypeSupport  = PartialFunction[SimpleDefinition, TypeSupport]

  def empty: CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => ComplexTypeSupport = _ => Map.empty
      override val getSimpleTypeSupport: GeneratorContext => SimpleTypeSupport = _ => Map.empty
    }
  }

  def complex(f: GeneratorContext => ComplexTypeSupport): CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => ComplexTypeSupport = f
      override val getSimpleTypeSupport: GeneratorContext => SimpleTypeSupport = _ => Map.empty
    }
  }

  def simple(f: GeneratorContext => SimpleTypeSupport): CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => ComplexTypeSupport = _ => Map.empty
      override val getSimpleTypeSupport: GeneratorContext => SimpleTypeSupport = f
    }
  }

  /**
    * Use org.joda.time.LocalDate for ''date'' type
    *
    * @param pattern read/write date format pattern (default is "yyyy-MM-dd")
    * @param corrector a simple string transformation function that can be used to transform input String before parsing.
    */
  def jodaLocalDateSupport(pattern: String = "yyyy-MM-dd", corrector: Tree = REF("identity")): CustomTypeSupport = simple { _ => {
    case _: DateDefinition =>
      val LocalDateClass = RootClass.newClass("org.joda.time.LocalDate")
      val defs = TypeSupportDefs(
        symbol = LocalDateClass,
        definition = EmptyTree,
        jsonReads  = {
          val readsType = RootClass.newClass("Reads") TYPE_OF LocalDateClass
          VAL("LocalDateReads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
            REF("Reads") DOT "jodaLocalDateReads" APPLY(LIT(pattern), corrector)
          }
        },
        jsonWrites = {
          val writesType = RootClass.newClass("Writes") TYPE_OF LocalDateClass
          VAL("LocalDateWrites", writesType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
            REF("Writes") DOT "jodaLocalDateWrites" APPLY LIT(pattern)
          }
        },
        queryBindable = EmptyTree,
        pathBindable  = EmptyTree
      )
      TypeSupport(LocalDateClass, LocalDateClass, defs :: Nil)
  }}

  /**
    * Use org.joda.time.DateTime for ''date-time'' type
    *
    * @param pattern read/write date-time format pattern (default is "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    * @param corrector a simple string transformation function that can be used to transform input String before parsing.
    */
  def jodaDateTimeSupport(pattern: String = "yyyy-MM-dd'T'HH:mm:ss.SSSXXX", corrector: Tree = REF("identity")): CustomTypeSupport = simple { _ => {
    case _: DateTimeDefinition =>
      val DateTimeClass = RootClass.newClass("org.joda.time.DateTime")
      val defs = TypeSupportDefs(
        symbol = DateTimeClass,
        definition = EmptyTree,
        jsonReads  = {
          val readsType = RootClass.newClass("Reads") TYPE_OF DateTimeClass
          VAL("DateTimeReads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
            REF("Reads") DOT "jodaDateReads" APPLY(LIT(pattern), corrector)
          }
        },
        jsonWrites = {
          val writesType = RootClass.newClass("Writes") TYPE_OF DateTimeClass
          VAL("DateTimeWrites", writesType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
            REF("Writes") DOT "jodaDateWrites" APPLY LIT(pattern)
          }
        },
        queryBindable = EmptyTree,
        pathBindable  = EmptyTree
      )
      TypeSupport(DateTimeClass, DateTimeClass, defs :: Nil)
  }}

  def emptyObjectAsJsObject: CustomTypeSupport = complex { _ => {
    case (obj: ObjectDefinition, _) if obj.properties.isEmpty =>
      TypeSupport(
        tpe = RootClass.newClass("play.api.libs.json.JsObject"),
        fullQualified = RootClass.newClass("play.api.libs.json.JsObject"),
        defs = Nil
      )
  }}

}
