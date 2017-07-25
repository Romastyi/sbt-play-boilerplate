package play.boilerplate.parser.backend.swagger

import io.swagger.models.parameters.{AbstractSerializableParameter, Parameter => SwaggerParameter}
import io.swagger.models.properties.{ArrayProperty, PropertyBuilder, Property => SwaggerProperty}
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ParameterParser { this: ModelParser with PropertyParser with ReferenceParser =>

  protected def parseParameter(schema: Schema, parameter: SwaggerParameter)
                              (implicit ctx: ParserContext): Parameter = {

    import io.swagger.models.{parameters => swagger}

    Option(parameter).getOrElse {
      throw ParserException("Trying to resolve null parameter.")
    } match {
      case param: swagger.BodyParameter =>
        BodyParameterFactory.build(parseModel(schema, getParamName(parameter), param.getSchema))
      case param: swagger.HeaderParameter =>
        parseTypedParameter(schema, param, HeaderParameterFactory)
      case param: swagger.PathParameter =>
        parseTypedParameter(schema, param, PathParameterFactory)
      case param: swagger.QueryParameter =>
        parseTypedParameter(schema, param, QueryParameterFactory)
      case param: swagger.FormParameter =>
        parseTypedParameter(schema, param, FormParameterFactory)
      case param: swagger.RefParameter =>
        RefParameterFactory.build(findReferenceDef(schema, param.get$ref()))
      case param =>
        throw ParserException(s"Unsupported parameter type (${param.getClass.getName}).")
    }

  }

  private def getParamName(parameter: SwaggerParameter): String = {
    Option(parameter.getName).getOrElse {
      throw ParserException("Parameter name is not specified.")
    }
  }

  object TypedParameter {
    def unapply(arg: SwaggerParameter): Option[AbstractSerializableParameter[_]] = {
      arg match {
        case param: AbstractSerializableParameter[_] =>
          Some(param)
        case _ =>
          None
      }
    }
  }

  object OptionParameter {
    def unapply(arg: SwaggerParameter): Option[AbstractSerializableParameter[_]] = {
      arg match {
        case TypedParameter(param) if Option(param.getRequired).forall(_ == false) =>
          Some(param)
        case _ =>
          None
      }
    }
  }

  object EnumParameter {
    def unapply(arg: SwaggerParameter): Option[(AbstractSerializableParameter[_], Iterable[String])] = {
      arg match {
        case TypedParameter(param) if Option(param.getEnum).isDefined =>
          Some((param, param.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  object ArrayParameter {
    def unapply(arg: SwaggerParameter): Option[(AbstractSerializableParameter[_], SwaggerProperty)] = {
      arg match {
        case TypedParameter(param) if param.getType == ArrayProperty.TYPE =>
          val items = Option(param.getItems).getOrElse {
            throw ParserException(s"Array items property is not specified for parameter.")
          }
          Some((param, items))
        case _ =>
          None
      }
    }
  }

  private def parseTypedParameter(schema: Schema,
                                  parameter: AbstractSerializableParameter[_],
                                  factory: DefinitionFactory[Parameter],
                                  canBeOption: Boolean = true)
                                 (implicit ctx: ParserContext): Parameter = {
    parameter match {
      case OptionParameter(param) if canBeOption =>
        factory.build(OptionDefinition(
          name = Option(param.getName).getOrElse(getParamName(parameter)),
          base = parseTypedParameter(schema, param, factory, canBeOption = false)
        ))
      case ArrayParameter(param, prop) =>
        val name = Option(param.getName).getOrElse(getParamName(parameter))
        factory.build(ArrayDefinition(
          name = name,
          items = getPropertyDef(schema, name, prop, canBeOption = false),
          uniqueItems = Option(param.isUniqueItems).exists(_ == true),
          minItems = Option(param.getMinItems).map(Integer2int),
          maxItems = Option(param.getMaxItems).map(Integer2int)
        ))
      case EnumParameter(param, items) =>
        factory.build(EnumDefinition(
          items = items,
          name = Option(param.getName).getOrElse(getParamName(parameter)),
          title = None,
          description = Option(param.getDescription),
          readOnly = Option(param.isReadOnly).exists(_ == true),
          allowEmptyValue = Option(param.getAllowEmptyValue).exists(_ == true),
          default = None
        ))
      case param =>
        val name = Option(param.getName).getOrElse(getParamName(parameter))
        getPropertyFactoryDef(schema, name, param2Prop(param), factory, canBeOption)
    }
  }

  private def param2Prop(parameter: AbstractSerializableParameter[_]): SwaggerProperty = {

    import io.swagger.models.properties.PropertyBuilder.PropertyId._

    val args = Map(
      ENUM -> parameter.getEnum,
      PATTERN -> parameter.getPattern,
      MIN_ITEMS -> parameter.getMinItems,
      MAX_ITEMS -> parameter.getMaxItems,
      MIN_LENGTH -> parameter.getMinLength,
      MAX_LENGTH -> parameter.getMaxLength,
      MINIMUM -> parameter.getMinimum,
      MAXIMUM -> parameter.getMaximum,
      MULTIPLE_OF -> parameter.getMultipleOf,
      VENDOR_EXTENSIONS -> parameter.getVendorExtensions
    ).asJava

    val property = PropertyBuilder.build(parameter.getType, parameter.getFormat, args)
    property.setName(parameter.getName)
    property.setRequired(parameter.getRequired)
    property.setDescription(parameter.getDescription)
    Option(parameter.getDefault).foreach(d => property.setDefault(d.toString))
    property.setExample(parameter.getExample)
    property.setAllowEmptyValue(parameter.getAllowEmptyValue)

    property

  }

}
