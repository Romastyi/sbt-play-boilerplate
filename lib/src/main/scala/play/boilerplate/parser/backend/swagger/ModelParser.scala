package play.boilerplate.parser.backend.swagger

import io.swagger.models.properties.{ObjectProperty, StringProperty, Property => SwaggerProperty}
import io.swagger.models.{ArrayModel, ModelImpl, RefModel, Model => SwaggerModel, ComposedModel => SwaggerComposedModel}
import play.boilerplate.parser.backend.ParserException
import play.boilerplate.parser.model._

import scala.collection.JavaConverters._

trait ModelParser { this: PropertyParser with ReferenceParser =>

  object ArrayModel {
    def unapply(arg: SwaggerModel): Option[(ArrayModel, SwaggerProperty)] = {
      arg match {
        case model: ArrayModel =>
          val items = Option(model.getItems).getOrElse {
            throw ParserException(s"Array items property is not specified for definition.")
          }
          Some((model, items))
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
    def unapply(arg: SwaggerModel): Option[(ModelImpl, Map[String, SwaggerProperty])] = {
      arg match {
        case model: ModelImpl if Option(model.getType).isEmpty || model.getType == ObjectProperty.TYPE =>
          val properties = Option(model.getProperties).map(_.asScala.toMap).getOrElse(Map.empty)
          for (name <- Option(model.getRequired).map(_.asScala).getOrElse(Nil)) {
            properties.get(name).foreach(_.setRequired(true))
          }
          Some((model, properties))
        case _ =>
          None
      }
    }
  }

  object ComposedModel {
    def unapply(arg: SwaggerModel): Option[(Seq[RefModel], Seq[ModelImpl])] = {
      arg match {
        case model: SwaggerComposedModel =>
          val interfaces = Option(model.getInterfaces).map(_.asScala).getOrElse(Nil)
          val inlines = Option(model.getAllOf)
            .map(_.asScala.filter(_.isInstanceOf[ModelImpl]))
            .getOrElse(Nil)
            .map(_.asInstanceOf[ModelImpl])
          Some((interfaces, inlines))
        case _ =>
          None
      }
    }
  }

  protected def parseModel(schema: Schema, modelName: String, model: SwaggerModel)
                          (implicit ctx: ParserContext): Model = {

    Option(model).getOrElse {
      throw ParserException("Trying to resolve null model.")
    } match {
      case ArrayModel(m, items) =>
        ModelFactory.build(ArrayDefinition(
          name = modelName,
          description = Option(m.getDescription),
          items = getPropertyDef(schema, modelName, items, canBeOption = false),
          uniqueItems = false,
          minItems = Option(m.getMinItems).map(Integer2int),
          maxItems = Option(m.getMaxItems).map(Integer2int),
          collectionFormat = CollectionFormat.None
        ))
      case EnumModel(m, items) =>
        ModelFactory.build(EnumDefinition(
          items = items,
          name = Option(m.getName).getOrElse(modelName),
          title = Option(m.getTitle),
          description = Option(m.getDescription),
          readOnly = false,
          allowEmptyValue = Option(m.getAllowEmptyValue).exists(_ == true),
          default = None
        ))
      case ObjectModel(m, properties) =>
        ModelFactory.build(ObjectDefinition(
          properties = for ((name, prop) <- properties) yield {
            name -> getPropertyDef(schema, name, prop)
          },
          name = Option(m.getName).getOrElse(modelName),
          title = Option(m.getTitle),
          description = Option(m.getDescription),
          readOnly = false,
          allowEmptyValue = Option(m.getAllowEmptyValue).exists(_ == true)
        ))
      case ComposedModel(interfaces, inlines) =>
        ModelFactory.build(ComplexObjectDefinition(
          interfaces = interfaces.map(
            ref => findReferenceDef(schema, ref.get$ref())
          ),
          inlines = inlines.map(parseModel(schema, "", _)),
          name = modelName,
          title = Option(model.getTitle),
          description = Option(model.getDescription),
          allowEmptyValue = false
        ))
      case ref: RefModel =>
        ModelFactory.build(findReferenceDef(schema, ref.get$ref()))
      case m =>
        throw ParserException(s"Unsupported parameter type (${m.getClass.getName}).")
    }

  }

}
