package play.boilerplate.parser.backend.swagger

import io.swagger.models.properties.{ObjectProperty, Property => SwaggerProperty, StringProperty}
import io.swagger.models.{ArrayModel, ModelImpl, RefModel, Model => SwaggerModel}
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ModelParser { this: PropertyParser with ReferenceParser =>

  object ArrayModel {
    def unapply(arg: SwaggerModel): Option[(ArrayModel, SwaggerProperty)] = {
      arg match {
        case model: ArrayModel =>
          Some((model, model.getItems))
        case _ =>
          None
      }
    }
  }

  object TypedModel {
    def unapply(arg: SwaggerModel): Option[(ModelImpl, String)] = {
      arg match {
        case model: ModelImpl if Option(model.getType).isDefined && model.getType != ObjectProperty.TYPE =>
          Some((model, model.getType))
        case _ =>
          None
      }
    }
  }

  object EnumModel {
    def unapply(arg: SwaggerModel): Option[(ModelImpl, Iterable[String])] = {
      arg match {
        case TypedModel(modelImpl, StringProperty.TYPE) if Option(modelImpl.getEnum).isDefined =>
          Some((modelImpl, modelImpl.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  object ObjectModel {
    def unapply(arg: SwaggerModel): Option[ModelImpl] = {
      arg match {
        case model: ModelImpl if Option(model.getType).isEmpty || model.getType == ObjectProperty.TYPE =>
          Some(model)
        case _ =>
          None
      }
    }
  }

  object ModelFactory extends DefinitionFactory[Definition with Model] {
    override def get(definition: Definition): Definition with Model = {
      new RefDefinition(definition.name, definition) with Model
    }
  }

  protected def parseModel(schema: Schema, model: SwaggerModel): Definition with Model = {

    Option(model).getOrElse {
      throw new NullPointerException("Trying to resolve null model.")
    } match {
      case ArrayModel(m, items) =>
        ModelFactory.get(ArrayDefinition(
          name = "",
          items = getPropertyDef(schema, items),
          uniqueItems = false,
          minLength = Option(m.getMaxItems).map(Integer2int),
          maxLength = Option(m.getMaxItems).map(Integer2int)
        ))
      case EnumModel(m, items) =>
        ModelFactory.get(new EnumDefinition(
          items = items,
          name = Option(m.getName).getOrElse(""),
          format = Option(m.getFormat),
          title = Option(m.getTitle),
          description = Option(m.getDescription),
          readOnly = false,
          allowEmptyValue = Option(m.getAllowEmptyValue).exists(_ == true),
          default = None
        ))
      case ObjectModel(m) =>
        ModelFactory.get(new ObjectDefinition(
          properties = Map.empty,
          name = Option(m.getName).getOrElse(""),
          format = Option(m.getFormat),
          title = Option(m.getTitle),
          description = Option(m.getDescription),
          readOnly = false,
          allowEmptyValue = Option(m.getAllowEmptyValue).exists(_ == true)
        ))
      case ref: RefModel =>
        ModelFactory.get(findReferenceDef(schema, ref.get$ref()))
      case m =>
        throw new RuntimeException(s"Unsupported parameter type (${m.getClass.getName}).")
    }

  }

}
