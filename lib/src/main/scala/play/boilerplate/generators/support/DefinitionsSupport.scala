package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.generators.GeneratorUtils._
import play.boilerplate.parser.model._

trait DefinitionsSupport
  extends BaseTypesSupport
    with EnumerationsSupport
    with ObjectSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def getTypeSupport(definition: Definition, context: DefinitionContext = DefinitionContext.default)
                    (implicit ctx: GeneratorContext): TypeSupport = {
    definition match {
      case OptionDefinition(_, base) =>
        val support = getTypeSupport(base, context)
        if (context.canBeOption) {
          support.copy(
            tpe = OptionClass TYPE_OF support.tpe,
            fullQualified = OptionClass TYPE_OF support.fullQualified,
            constructor = l => SOME(support.constructor(l))
          )
        } else {
          support
        }
      case ArrayDefinition(_, _, items, _, _, _, collectionFormat) =>
        val support = getTypeSupport(items, context)
        val listDefs = TypeSupportDefs(
          symbol = (ListClass TYPE_OF support.tpe).typeSymbol,
          definition = EmptyTree,
          jsonReads = EmptyTree,
          jsonWrites = EmptyTree,
          queryBindable = getCollectFormatQueryBinder(support.tpe, collectionFormat),
          pathBindable = EmptyTree
        )
        support.copy(
          tpe = ListClass TYPE_OF support.tpe,
          fullQualified = ListClass TYPE_OF support.fullQualified,
          defs = support.defs :+ listDefs,
          constructor = l => LIST(support.constructor(l))
        )
      case MapDefinition(_, _, additionalProperties) =>
        val support = getTypeSupport(additionalProperties, context)
        support.copy(
          tpe = ImmutableMapClass TYPE_OF (StringClass, support.tpe),
          fullQualified = ImmutableMapClass TYPE_OF (StringClass, support.fullQualified),
          constructor = l => MAKE_MAP(LIT("") INFIX (" -> ", support.constructor(l)))
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

  private def customListQueryBinder(tpe: Type, separator: Char): Tree = {
    val queryBindableType = RootClass.newClass("QueryStringBindable") TYPE_OF (ListClass TYPE_OF tpe)
    val tpeName = stringToValidIdentifier(tpe.safeToString, skipNotValidChars = true)
    VAL(s"${tpeName}ListQueryBindable", queryBindableType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      RootClass.newClass("play.boilerplate.api.common.Binders") DOT "queryList" APPLYTYPE tpe APPLY LIT(separator)
    }
  }

  private def getCollectFormatQueryBinder(tpe: Type, collectionFormat: CollectionFormat): Tree = collectionFormat match {
    case CollectionFormat.None  => EmptyTree
    case CollectionFormat.Multi => EmptyTree // default Play behavior
    case CollectionFormat.Csv   => customListQueryBinder(tpe, ',' )
    case CollectionFormat.Ssv   => customListQueryBinder(tpe, ' ' )
    case CollectionFormat.Tsv   => customListQueryBinder(tpe, '\t')
    case CollectionFormat.Pipes => customListQueryBinder(tpe, '|' )
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
        getTypeSupport(m.ref)(ctx.setInModel(true).setIsModel(true).setNeedInterface(m.isInterface))
      case p: Parameter =>
        getTypeSupport(p.ref)(ctx.setIsModel(false))
      case RefDefinition(_, ref) =>
        getTypeSupport(ref)(ctx.setIsModel(false))
    }
  }

}
