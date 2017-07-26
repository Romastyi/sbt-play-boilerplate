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

  def getTypeSupport(definition: Definition, context: DefinitionContext = DefinitionContext.inline)
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
          tpe = ImmutableMapClass TYPE_OF (StringClass, support.tpe),
          fullQualified = ImmutableMapClass TYPE_OF (StringClass, support.fullQualified)
        )
      case ref: RefDefinition =>
        getTypeSupportRef(ref)
      case simple: SimpleDefinition =>
        ctx.settings.customTypeSupport.getSimpleTypeSupport(ctx).lift(simple).getOrElse {
          getSimpleTypeSupport(simple)
        }
      case complex: ComplexDefinition =>
        ctx.settings.customTypeSupport.getComplexTypeSupport(ctx).lift((complex, context)).getOrElse {
          getComplexTypeSupport(complex, context)
        }
      case _ =>
        throw new RuntimeException(s"Unsupported definition type ${definition.getClass.getName}.")
    }
  }

  private def getComplexTypeSupport(definition: ComplexDefinition, context: DefinitionContext)
                                   (implicit cxt: GeneratorContext): TypeSupport = {
    definition match {
      case obj: ObjectDefinition =>
        getObjectSupport(obj, context)
      case enum: EnumDefinition =>
        getEnumSupport(enum, context)
      case complex: ComplexObjectDefinition =>
        getComplexObjectSupport(complex, context)
    }
  }

  def getTypeSupportRef(reference: RefDefinition)(implicit ctx: GeneratorContext): TypeSupport = {
    reference match {
      case m: Model =>
        val context = if (ctx.inModel || ctx.inService || ctx.inClient) {
          DefinitionContext.withoutDefinition
        } else {
          DefinitionContext.inline
        }
        val newCtx = ctx.setInModel(true).setIsModel(true).setNeedInterface(m.isInterface)
        getTypeSupport(m.ref, context)(newCtx)
      case p: Parameter =>
        val context = if (ctx.inModel) {
          DefinitionContext.withoutDefinition
        } else {
          DefinitionContext.inline
        }
        getTypeSupport(p.ref, context)(ctx.setIsModel(false))
      case RefDefinition(_, ref) =>
        getTypeSupport(ref, DefinitionContext.inline)(ctx.setIsModel(false))
    }
  }

}
