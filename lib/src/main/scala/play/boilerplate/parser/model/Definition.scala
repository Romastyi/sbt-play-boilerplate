package play.boilerplate.parser.model

sealed trait Definition extends WithResolve[Definition] {
  def name: String
  def baseDef: Definition = this
  override def resolve(resolver: DefinitionResolver): Definition = this
}

trait DefinitionFactory[D <: Definition] {
  def get(definition: Definition): D
}

final case class OptionDefinition(override val name: String,
                                  base: Definition) extends Definition {
  override val baseDef: Definition = base.baseDef
  override def resolve(resolver: DefinitionResolver): Definition = {
    copy(base = base.resolve(resolver))
  }
}
final case class ArrayDefinition(override val name: String,
                                 items: Definition,
                                 uniqueItems: Boolean,
                                 override val minLength: Option[Int],
                                 override val maxLength: Option[Int]
                                ) extends Definition with WithMinMaxLength {
  override val baseDef: Definition = items.baseDef
  override def resolve(resolver: DefinitionResolver): Definition = {
    copy(items = items.resolve(resolver))
  }
}
case class RefDefinition(override val name: String,
                         ref: Definition) extends Definition {
  override val baseDef: Definition = ref.baseDef
  override def resolve(resolver: DefinitionResolver): Definition = {
    copy(ref = ref.resolve(resolver))
  }
}

final case class LazyRefDefinition(ref: String) extends Definition {
  override val name: String = ""
  override def resolve(resolver: DefinitionResolver): Definition = {
    resolver.resolveByRef(ref)
  }
}

final case class MapDefinition(override val name: String,
                               additionalProperties: Definition
                              ) extends Definition {
  override val baseDef: Definition = additionalProperties.baseDef
  override def resolve(resolver: DefinitionResolver): Definition = {
    copy(additionalProperties = additionalProperties.resolve(resolver))
  }
}

sealed trait DefinitionImpl extends Definition {

  //def xml: Xml
  //def required: Boolean
  //def position: Int <-- Property
  def title: Option[String]
  def description: Option[String]
  //def example: Object
  //def externalDocs: ExternalDocs <-- Model
  //def reference: String <-- Model
  //def readOnly: Boolean
  def allowEmptyValue: Boolean
  //def access: Option[String] <-- Property

}

final case class ObjectDefinition(properties: Map[String, Definition],
                                  override val name: String,
                                  override val title: Option[String],
                                  override val description: Option[String],
                                  override val readOnly: Boolean,
                                  override val allowEmptyValue: Boolean
                                 ) extends DefinitionImpl with WithReadOnly {
  override def resolve(resolver: DefinitionResolver): Definition = {
    copy(
      properties = for ((name, prop) <- properties) yield name -> prop.resolve(resolver)
    )
  }
}

final case class EnumDefinition(items: Iterable[String],
                                override val name: String,
                                override val title: Option[String],
                                override val description: Option[String],
                                override val readOnly: Boolean,
                                override val allowEmptyValue: Boolean,
                                override val default: Option[String]
                               ) extends DefinitionImpl with WithReadOnly with WithDefault[String]

final case class StringDefinition(override val name: String,
                                  override val title: Option[String],
                                  override val description: Option[String],
                                  override val readOnly: Boolean,
                                  override val allowEmptyValue: Boolean,
                                  override val default: Option[String],
                                  override val minLength: Option[Int],
                                  override val maxLength: Option[Int],
                                  override val pattern: Option[String]
                                 ) extends DefinitionImpl with WithReadOnly with WithDefault[String] with WithMinMaxLength with WithPattern

final case class BooleanDefinition(override val name: String,
                                   override val title: Option[String],
                                   override val description: Option[String],
                                   override val readOnly: Boolean,
                                   override val allowEmptyValue: Boolean,
                                   override val default: Option[Boolean]
                                  ) extends DefinitionImpl with WithReadOnly with WithDefault[Boolean]

final case class DoubleDefinition(override val name: String,
                                  override val title: Option[String],
                                  override val description: Option[String],
                                  override val readOnly: Boolean,
                                  override val allowEmptyValue: Boolean,
                                  override val default: Option[Double]
                                 ) extends DefinitionImpl with WithReadOnly with WithDefault[Double]

final case class FloatDefinition(override val name: String,
                                 override val title: Option[String],
                                 override val description: Option[String],
                                 override val readOnly: Boolean,
                                 override val allowEmptyValue: Boolean,
                                 override val default: Option[Float]
                                ) extends DefinitionImpl with WithReadOnly with WithDefault[Float]

final case class IntegerDefinition(override val name: String,
                                   override val title: Option[String],
                                   override val description: Option[String],
                                   override val readOnly: Boolean,
                                   override val allowEmptyValue: Boolean,
                                   override val default: Option[Int]
                                  ) extends DefinitionImpl with WithReadOnly with WithDefault[Int]

final case class LongDefinition(override val name: String,
                                override val title: Option[String],
                                override val description: Option[String],
                                override val readOnly: Boolean,
                                override val allowEmptyValue: Boolean,
                                override val default: Option[Long]
                               ) extends DefinitionImpl with WithReadOnly with WithDefault[Long]

final case class DecimalDefinition(override val name: String,
                                   override val title: Option[String],
                                   override val description: Option[String],
                                   override val readOnly: Boolean,
                                   override val allowEmptyValue: Boolean
                                  ) extends DefinitionImpl with WithReadOnly

final case class DateDefinition(override val name: String,
                                override val title: Option[String],
                                override val description: Option[String],
                                override val readOnly: Boolean,
                                override val allowEmptyValue: Boolean
                               ) extends DefinitionImpl with WithReadOnly

final case class DateTimeDefinition(override val name: String,
                                    override val title: Option[String],
                                    override val description: Option[String],
                                    override val readOnly: Boolean,
                                    override val allowEmptyValue: Boolean
                                   ) extends DefinitionImpl with WithReadOnly

final case class UUIDDefinition(override val name: String,
                                override val title: Option[String],
                                override val description: Option[String],
                                override val readOnly: Boolean,
                                override val allowEmptyValue: Boolean,
                                override val default: Option[String],
                                override val pattern: Option[String]
                               ) extends DefinitionImpl with WithReadOnly with WithDefault[String] with WithPattern

final case class FileDefinition(override val name: String,
                                override val title: Option[String],
                                override val description: Option[String],
                                override val readOnly: Boolean,
                                override val allowEmptyValue: Boolean
                               ) extends DefinitionImpl with WithReadOnly
