package play.boilerplate.parser.backend.swagger

import io.swagger.models.properties.{Property => SwaggerProperty, _}
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait PropertyParser { this: ReferenceParser =>

  object OptionProperty {
    def unapply(arg: SwaggerProperty): Option[SwaggerProperty] = {
      Option(arg.getRequired) match {
        case Some(false) =>
          arg.setRequired(true)
          Some(arg)
        case _ =>
          None
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

  object PropertyFactory extends DefinitionFactory[Definition with Property] {
    override def get(definition: Definition): Definition with Property = {
      new RefDefinition(definition.name, definition) with Property
    }
  }

  protected def getPropertyFactoryDef[D <: Definition](schema: Schema,
                                                       property: SwaggerProperty,
                                                       factory: DefinitionFactory[D]): D = {
    factory.get(getPropertyDef(schema, property))
  }

  protected def getPropertyDef(schema: Schema, property: SwaggerProperty): Definition = {

    Option(property).getOrElse {
      throw new NullPointerException("Trying to resolve null property.")
    } match {
      case OptionProperty(prop) =>
        OptionDefinition(
          name = Option(prop.getName).getOrElse(""),
          base = getPropertyDef(schema, prop)
        )
      case EnumProperty(prop, items) =>
        new EnumDefinition(
          items = items,
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault)
        )
      case prop: StringProperty =>
        new StringDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
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
        new BooleanDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Boolean2boolean)
        )
      case prop: DoubleProperty =>
        new DoubleDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Double2double)
        )
      case prop: FloatProperty =>
        new FloatDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Float2float)
        )
      case prop: IntegerProperty =>
        new IntegerDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Integer2int)
        )
      case prop: LongProperty =>
        new LongDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault).map(Long2long)
        )
      case prop: BaseIntegerProperty =>
        new IntegerDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = None
        )
      case prop: DecimalProperty =>
        new DecimalDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: DateProperty =>
        new DateDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: DateTimeProperty =>
        new DateTimeDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: UUIDProperty =>
        new UUIDDefinition(
          name = Option(prop.getName).getOrElse(""),
          format = Option(prop.getFormat),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true),
          default = Option(prop.getDefault),
          pattern = Option(prop.getPattern)
        )
      case prop: FileProperty =>
        new FileDefinition(
          name = Option(prop.getName).getOrElse(""),
          title = Option(prop.getTitle),
          description = Option(prop.getDescription),
          readOnly = Option(prop.getReadOnly).exists(_ == true),
          allowEmptyValue = Option(prop.getAllowEmptyValue).exists(_ == true)
        )
      case prop: MapProperty =>
        MapDefinition(
          name = Option(prop.getName).getOrElse(""),
          additionalProperties = getPropertyDef(schema, prop.getAdditionalProperties)
        )
      case prop: ArrayProperty =>
        ArrayDefinition(
          name = Option(prop.getName).getOrElse(""),
          items = getPropertyDef(schema, prop.getItems),
          uniqueItems = Option(prop.getUniqueItems).exists(_ == true),
          minLength = Option(prop.getMinItems).map(Integer2int),
          maxLength = Option(prop.getMaxItems).map(Integer2int)
        )
      case prop: RefProperty =>
        findReferenceDef(schema, prop.get$ref())
      case prop =>
        throw new RuntimeException(s"Unsupported property type (${prop.getClass.getName}).")
    }

  }

}
