package play.boilerplate.parser.backend.swagger

import io.swagger.models.parameters.{AbstractSerializableParameter, Parameter => SwaggerParameter}
import io.swagger.models.properties.{ArrayProperty, PropertyBuilder, Property => SwaggerProperty}
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ParameterParser { this: ModelParser with PropertyParser with ReferenceParser =>

  protected def parseParameter(schema: Schema, parameter: SwaggerParameter): Definition with Parameter = {

    import io.swagger.models.{parameters => swagger}

    Option(parameter).getOrElse {
      throw new NullPointerException("Trying to resolve null parameter.")
    } match {
      case param: swagger.BodyParameter =>
        BodyParameterFactory.get(parseModel(schema, param.getSchema))
      case param: swagger.HeaderParameter =>
        parseTypedParameter(schema, param, HeaderParameterFactory)
      case param: swagger.PathParameter =>
        parseTypedParameter(schema, param, PathParameterFactory)
      case param: swagger.QueryParameter =>
        parseTypedParameter(schema, param, QueryParameterFactory)
      case param: swagger.FormParameter =>
        parseTypedParameter(schema, param, FormParameterFactory)
      case param: swagger.RefParameter =>
        RefsParameterFactory.get(findReferenceDef(schema, param.get$ref()))
      case param =>
        throw new RuntimeException(s"Unsupported parameter type (${param.getClass.getName}).")
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
        case TypedParameter(param) if Option(param.getRequired).exists(_ == false) =>
          param.setRequired(true)
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
          Some((param, param.getItems))
        case _ =>
          None
      }
    }
  }

  object RefsParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with RefParameter
    }
  }

  object BodyParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with BodyParameter
    }
  }

  object HeaderParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with HeaderParameter
    }
  }

  object PathParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with PathParameter
    }
  }

  object QueryParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with QueryParameter
    }
  }

  object FormParameterFactory extends DefinitionFactory[Definition with Parameter] {
    override def get(definition: Definition): Definition with Parameter = {
      new RefDefinition(definition.name, definition) with FormParameter
    }
  }

  private def parseTypedParameter(schema: Schema,
                                  parameter: AbstractSerializableParameter[_],
                                  factory: DefinitionFactory[Definition with Parameter]): Definition with Parameter = {
    parameter match {
      case OptionParameter(param) =>
        factory.get(parseTypedParameter(schema, param, factory))
      case ArrayParameter(param, prop) =>
        factory.get(ArrayDefinition(
          name = Option(param.getName).getOrElse(""),
          items = getPropertyDef(schema, prop),
          uniqueItems = Option(param.isUniqueItems).exists(_ == true),
          minLength = Option(param.getMinLength).map(Integer2int),
          maxLength = Option(param.getMaxLength).map(Integer2int)
        ))
      case EnumParameter(param, items) =>
        factory.get(new EnumDefinition(
          items = items,
          name = Option(param.getName).getOrElse(""),
          format = Option(param.getFormat),
          title = None,
          description = Option(param.getDescription),
          readOnly = Option(param.isReadOnly).exists(_ == true),
          allowEmptyValue = Option(param.getAllowEmptyValue).exists(_ == true),
          default = None
        ))
      case param =>
        getPropertyFactoryDef(schema, param2Prop(param), factory)
    }
  }

  private def param2Prop(parameter: AbstractSerializableParameter[_]): SwaggerProperty = {

    import io.swagger.models.properties.PropertyBuilder.PropertyId._

    val args = Map(
      ENUM -> parameter.getEnum,
      DESCRIPTION -> parameter.getDescription,
      DEFAULT -> parameter.getDefault,
      PATTERN -> parameter.getPattern,
      MIN_ITEMS -> parameter.getMinItems,
      MAX_ITEMS -> parameter.getMaxItems,
      MIN_LENGTH -> parameter.getMinLength,
      MAX_LENGTH -> parameter.getMaxLength,
      MINIMUM -> parameter.getMinimum,
      MAXIMUM -> parameter.getMaximum,
      EXAMPLE -> parameter.getExample,
      TYPE -> parameter.getType,
      FORMAT -> parameter.getFormat,
      REQUIRED -> boolean2Boolean(Option(parameter.getRequired).getOrElse(false)),
      VENDOR_EXTENSIONS -> parameter.getVendorExtensions,
      ALLOW_EMPTY_VALUE -> parameter.getAllowEmptyValue,
      MULTIPLE_OF -> parameter.getMultipleOf
    ).asJava

    PropertyBuilder.build(parameter.getType, parameter.getFormat, args)

  }

}
