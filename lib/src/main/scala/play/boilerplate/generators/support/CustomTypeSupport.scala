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

  def jodaLocalDateSupport: CustomTypeSupport = {
    simple { _ => {
      case _: DateDefinition =>
        val LocalDateClass = definitions.getClass("org.joda.time.LocalDate")
        TypeSupport(LocalDateClass, LocalDateClass, Nil)
    }}
  }

  def jodaDateTimeSupport(pattern: String = "yyyy-MM-dd'T'HH:mm:ss.SSSXXX"): CustomTypeSupport = {
    simple { _ => {
      case _: DateTimeDefinition =>
        val DateTimeClass  = definitions.getClass("org.joda.time.DateTime")
        val defs = TypeSupportDefs(
          symbol = DateTimeClass,
          definition = EmptyTree,
          jsonReads  = {
            val readsType = definitions.getClass("Reads") TYPE_OF DateTimeClass
            VAL(DateTimeClass.nameString + "Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
              REF("Reads") DOT "jodaDateReads" APPLY LIT(pattern)
            }
          },
          jsonWrites = {
            val writesType = definitions.getClass("Writes") TYPE_OF DateTimeClass
            VAL(DateTimeClass.nameString + "Writes", writesType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
              REF("Writes") DOT "jodaDateWrites" APPLY LIT(pattern)
            }
          },
          queryBindable = EmptyTree,
          pathBindable  = EmptyTree
        )
        TypeSupport(DateTimeClass, DateTimeClass, defs :: Nil)
    }}
  }

}
