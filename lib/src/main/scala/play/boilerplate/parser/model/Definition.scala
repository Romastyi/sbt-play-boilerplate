package play.boilerplate.parser.model

sealed trait Definition {
  def name: String
  def baseDef: Definition = this
}

trait DefinitionFactory[D <: Definition] {
  def get(definition: Definition): D
}

final case class OptionDefinition(override val name: String,
                                  base: Definition) extends Definition {
  override val baseDef: Definition = base.baseDef
}
final case class ArrayDefinition(override val name: String,
                                 items: Definition,
                                 uniqueItems: Boolean,
                                 override val minLength: Option[Int],
                                 override val maxLength: Option[Int]
                                ) extends Definition with WithMinMaxLength {
  override val baseDef: Definition = items.baseDef
}
case class RefDefinition(override val name: String,
                         ref: Definition) extends Definition {
  override val baseDef: Definition = ref.baseDef
}

case class MapDefinition(override val name: String,
                         additionalProperties: Definition
                        ) extends Definition {
  override val baseDef: Definition = additionalProperties.baseDef
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

final class ObjectDefinition(properties: Map[String, Definition],
                             override val name: String,
                             override val format: Option[String],
                             override val title: Option[String],
                             override val description: Option[String],
                             override val readOnly: Boolean,
                             override val allowEmptyValue: Boolean
                            ) extends DefinitionImpl with WithFormat with WithReadOnly

final class EnumDefinition(items: Iterable[String],
                           override val name: String,
                           override val format: Option[String],
                           override val title: Option[String],
                           override val description: Option[String],
                           override val readOnly: Boolean,
                           override val allowEmptyValue: Boolean,
                           override val default: Option[String]
                          ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[String]

final class StringDefinition(override val name: String,
                             override val format: Option[String],
                             override val title: Option[String],
                             override val description: Option[String],
                             override val readOnly: Boolean,
                             override val allowEmptyValue: Boolean,
                             override val default: Option[String],
                             override val minLength: Option[Int],
                             override val maxLength: Option[Int],
                             override val pattern: Option[String]
                            ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[String] with WithMinMaxLength with WithPattern

final class BooleanDefinition(override val name: String,
                              override val format: Option[String],
                              override val title: Option[String],
                              override val description: Option[String],
                              override val readOnly: Boolean,
                              override val allowEmptyValue: Boolean,
                              override val default: Option[Boolean]
                             ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[Boolean]

final class DoubleDefinition(override val name: String,
                             override val format: Option[String],
                             override val title: Option[String],
                             override val description: Option[String],
                             override val readOnly: Boolean,
                             override val allowEmptyValue: Boolean,
                             override val default: Option[Double]
                            ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[Double]

final class FloatDefinition(override val name: String,
                            override val format: Option[String],
                            override val title: Option[String],
                            override val description: Option[String],
                            override val readOnly: Boolean,
                            override val allowEmptyValue: Boolean,
                            override val default: Option[Float]
                           ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[Float]

final class IntegerDefinition(override val name: String,
                              override val format: Option[String],
                              override val title: Option[String],
                              override val description: Option[String],
                              override val readOnly: Boolean,
                              override val allowEmptyValue: Boolean,
                              override val default: Option[Int]
                             ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[Int]

final class LongDefinition(override val name: String,
                           override val format: Option[String],
                           override val title: Option[String],
                           override val description: Option[String],
                           override val readOnly: Boolean,
                           override val allowEmptyValue: Boolean,
                           override val default: Option[Long]
                          ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[Long]

final class DecimalDefinition(override val name: String,
                              override val format: Option[String],
                              override val title: Option[String],
                              override val description: Option[String],
                              override val readOnly: Boolean,
                              override val allowEmptyValue: Boolean
                             ) extends DefinitionImpl with WithFormat with WithReadOnly

final class DateDefinition(override val name: String,
                           override val format: Option[String],
                           override val title: Option[String],
                           override val description: Option[String],
                           override val readOnly: Boolean,
                           override val allowEmptyValue: Boolean
                          ) extends DefinitionImpl with WithFormat with WithReadOnly

final class DateTimeDefinition(override val name: String,
                               override val format: Option[String],
                               override val title: Option[String],
                               override val description: Option[String],
                               override val readOnly: Boolean,
                               override val allowEmptyValue: Boolean
                              ) extends DefinitionImpl with WithFormat with WithReadOnly

final class UUIDDefinition(override val name: String,
                           override val format: Option[String],
                           override val title: Option[String],
                           override val description: Option[String],
                           override val readOnly: Boolean,
                           override val allowEmptyValue: Boolean,
                           override val default: Option[String],
                           override val pattern: Option[String]
                          ) extends DefinitionImpl with WithFormat with WithReadOnly with WithDefault[String] with WithPattern

final class FileDefinition(override val name: String,
                           override val title: Option[String],
                           override val description: Option[String],
                           override val readOnly: Boolean,
                           override val allowEmptyValue: Boolean
                          ) extends DefinitionImpl with WithReadOnly
