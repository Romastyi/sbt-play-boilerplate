package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model._

trait DefinitionsSupport
  extends BaseTypesSupport
    with EnumerationsSupport
    with ObjectSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def getTypeSupport(definition: Definition, context: DefinitionContext = DefinitionContext.empty)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    definition match {
      case OptionDefinition(_, base) =>
        val support = getTypeSupport(base, context)
        support.copy(
          tpe = OptionClass TYPE_OF support.tpe,
          fullQualified = OptionClass TYPE_OF support.fullQualified
        )
      case ArrayDefinition(_, items, _, _, _) =>
        val support = getTypeSupport(items, context)
        support.copy(
          tpe = ListClass TYPE_OF support.tpe,
          fullQualified = ListClass TYPE_OF support.fullQualified
        )
      case MapDefinition(_, additionalProperties) =>
        val support = getTypeSupport(additionalProperties, context)
        support.copy(
          tpe = RootClass.newClass("Map") TYPE_OF (StringClass, support.tpe),
          fullQualified = RootClass.newClass("Map") TYPE_OF (StringClass, support.fullQualified)
        )
      case ref: RefDefinition =>
        getTypeSupportRef(ref)
      case impl: DefinitionImpl =>
        getTypeSupportImpl(impl, context)
      case _ =>
        throw new RuntimeException(s"Unsupported definition type ${definition.getClass.getName}.")
    }
  }

  private def getTypeSupportImpl(definitionImpl: DefinitionImpl, context: DefinitionContext)
                                (implicit cxt: GeneratorContext): TypeSupport = {
    definitionImpl match {
      case obj: ObjectDefinition =>
        getObjectSupport(obj, context)
      case enum: EnumDefinition =>
        getEnumSupport(enum, context)
      case str: StringDefinition =>
        getStringSupport(str)
      case bool: BooleanDefinition =>
        getBooleanSupport(bool)
      case double: DoubleDefinition =>
        getDoubleSupport(double)
      case float: FloatDefinition =>
        getFloatSupport(float)
      case int: IntegerDefinition =>
        getIntegerSupport(int)
      case long: LongDefinition =>
        getLongSupport(long)
      case decimal: DecimalDefinition =>
        getDecimalSupport(decimal)
      case date: DateDefinition =>
        getDateSupport(date)
      case dt: DateTimeDefinition =>
        getDateTimeSupport(dt)
      case uuid: UUIDDefinition =>
        getUUIDSupport(uuid)
      case file: FileDefinition =>
        getFileSupport(file)
    }
  }

  private def getTypeSupportRef(reference: RefDefinition)(implicit ctx: GeneratorContext): TypeSupport = {
    reference match {
      case m: Model =>
        getTypeSupport(m.ref, DefinitionContext.model)
      case p: Parameter =>
        getTypeSupport(p.ref, DefinitionContext.parameter)
      case p: Property =>
        getTypeSupport(p.ref, DefinitionContext.property)
      case RefDefinition(_, ref) =>
        getTypeSupport(ref, DefinitionContext.empty)
    }
  }

}
