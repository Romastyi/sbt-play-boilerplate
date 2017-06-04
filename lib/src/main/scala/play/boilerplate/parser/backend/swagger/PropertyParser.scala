package play.boilerplate.parser.backend.swagger

import io.swagger.models.properties.{Property => SwaggerProperty, _}
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait PropertyParser { this: ReferenceParser =>

  object OptionProperty {
    def unapply(arg: SwaggerProperty): Option[SwaggerProperty] = {
      if (Option(arg.getRequired).getOrElse(false)) {
        None
      } else {
        arg.setRequired(true)
        Some(arg)
      }
    }
  }

  object EnumProperty {
    def unapply(arg: SwaggerProperty): Option[(StringProperty, Iterable[String])] = {
      arg match {
        case prop: StringProperty if Option(prop.getEnum).isDefined =>
          Some((prop, prop.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  protected def getPropertyFactoryDef[D <: Definition](schema: Schema,
                                                       propertyName: String,
                                                       property: SwaggerProperty,
                                                       factory: DefinitionFactory[D])
                                                      (implicit ctx: ParserContext): D = {
    factory.build(getPropertyDef(schema, propertyName, property))
  }

  protected def getPropertyDef(schema: Schema, propertyName: String, property: SwaggerProperty)
                              (implicit ctx: ParserContext): Definition = {

    Option(property).getOrElse {
      throw ParserException("Trying to resolve null property.")
    } match {
      case OptionProperty(prop) =>
        OptionDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          base = getPropertyDef(schema, propertyName, prop)
        )
      case EnumProperty(prop, items) =>
        EnumDefinition(
          items = items,
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault)
        )
      case prop: StringProperty =>
        StringDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault),
          minLength = Option(prop.getMinLength).map(Integer2int),
          maxLength = Option(prop.getMaxLength).map(Integer2int),
          pattern = Option(prop.getPattern)
        )
      case prop: BooleanProperty =>
        BooleanDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Boolean2boolean)
        )
      case prop: DoubleProperty =>
        DoubleDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Double2double)
        )
      case prop: FloatProperty =>
        FloatDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Float2float)
        )
      case prop: IntegerProperty =>
        IntegerDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Integer2int)
        )
      case prop: LongProperty =>
        LongDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Long2long)
        )
      case prop: BaseIntegerProperty =>
        IntegerDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = None
        )
      case prop: DecimalProperty =>
        DecimalDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: DateProperty =>
        DateDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: DateTimeProperty =>
        DateTimeDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: UUIDProperty =>
        UUIDDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault),
          pattern = Option(prop.getPattern)
        )
      case prop: FileProperty =>
        FileDefinition(
          name = Option(prop.getName).getOrElse(propertyName),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: MapProperty =>
        val name = Option(prop.getName).getOrElse(propertyName)
        MapDefinition(
          name = name,
          additionalProperties = getPropertyDef(schema, name + "Items", prop.getAdditionalProperties)
        )
      case prop: ArrayProperty =>
        val name = Option(prop.getName).getOrElse(propertyName)
        val items = Option(prop.getItems).getOrElse {
          throw ParserException(s"Array items property is not specified for property.")
        }
        items.setRequired(true)
        ArrayDefinition(
          name = name,
          items = getPropertyDef(schema, name + "Items", items),
          uniqueItems = Option(prop.getUniqueItems).exists(_ == true),
          minLength = Option(prop.getMinItems).map(Integer2int),
          maxLength = Option(prop.getMaxItems).map(Integer2int)
        )
      case prop: RefProperty =>
        findReferenceDef(schema, prop.get$ref())
      case prop =>
        throw ParserException(s"Unsupported property type (${prop.getClass.getName}).")
    }

  }

}
