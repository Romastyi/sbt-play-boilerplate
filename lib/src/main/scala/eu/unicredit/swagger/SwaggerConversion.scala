/* Copyright 2015 UniCredit S.p.A.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package eu.unicredit.swagger

import io.swagger.models.{Model, ModelImpl}
import io.swagger.models.properties._
import io.swagger.models.parameters._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConverters._

trait SwaggerConversion {

  object TypeModel {
    def unapply(arg: Model): Option[(ModelImpl, String)] = {
      arg match {
        case model: ModelImpl if Option(model.getType).isDefined && model.getType != ObjectProperty.TYPE =>
          Some((model, model.getType))
        case _ =>
          None
      }
    }
  }

  object EnumerationModel {
    def unapply(arg: Model): Option[(ModelImpl, Iterable[String])] = {
      arg match {
        case TypeModel(modelImpl, StringProperty.TYPE) if Option(modelImpl.getEnum).isDefined =>
          Some((modelImpl, modelImpl.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  def propType(p: Property): Type = {
    propType(p, noOptPropType(p))
  }

  def propType(p: Property, tpe: Type): Type = {
    if (Option(p.getRequired).getOrElse(false))
      tpe
    else
      OptionClass TYPE_OF tpe
  }

  object EnumProperty {
    def unapply(arg: Property): Option[(StringProperty, Iterable[String])] = {
      arg match {
        case prop: StringProperty if Option(prop.getEnum).isDefined =>
          Some((prop, prop.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  private lazy val OffsetDateTimeClass =
    definitions.getClass("java.time.OffsetDateTime")

  private lazy val LocalDateClass =
    definitions.getClass("java.time.LocalDate")

  private lazy val UUIDClass =
    definitions.getClass("java.util.UUID")

  private lazy val EnumerationClass = definitions.getClass("Enumeration")

  def composeEnumName(modelName: String, fieldName: String): String = {
    StringUtils.toCamelCase(modelName) + StringUtils.toCamelCase(fieldName)
  }

  def enumerationValueType(enum: Symbol): Type = {
    TYPE_REF(enum DOT "Value")
  }

  def enumerationValueType(modelName: String, fieldName: String): Type = {
    enumerationValueType(definitions.getClass(composeEnumName(modelName, fieldName)))
  }

  def generateEnumeration(name: String, items: Iterable[String]): Tree = {

    OBJECTDEF(RootClass.newClass(name)) withParents EnumerationClass := BLOCK {
      items.map { item =>
        VAL(StringUtils.toCamelCase(item)) := REF("Value") APPLY LIT(item)
      }
    }

  }

  def noOptPropType(p: Property): Type = {
    p match {
      case EnumProperty(_, _) =>
        throw new Exception(s"Enums are not supported yet")
      case _: StringProperty =>
        StringClass
      case _: BooleanProperty =>
        BooleanClass
      case _: DoubleProperty =>
        DoubleClass
      case _: FloatProperty =>
        FloatClass
      case _: IntegerProperty =>
        IntClass
      case _: LongProperty =>
        LongClass
      case _: BaseIntegerProperty =>
        IntClass
      case m: MapProperty =>
        RootClass.newClass("Map") TYPE_OF (StringClass, noOptPropType(m.getAdditionalProperties))
      case a: ArrayProperty =>
        ListClass TYPE_OF noOptPropType(a.getItems)
      case _: DecimalProperty =>
        BigDecimalClass
      case r: RefProperty =>
        RootClass.newClass(r.getSimpleRef)
      case _: DateProperty =>
        LocalDateClass
      case _: DateTimeProperty =>
        OffsetDateTimeClass
      case _: UUIDProperty =>
        UUIDClass

      case ba: ByteArrayProperty =>
        throw new Exception(s"ByteArrayProperty $p is not supported yet")
      case b: BinaryProperty =>
        throw new Exception(s"BinaryProperty $p is not supported yet")
      // supported as a subclass of StringProperty
      //case e: EmailProperty =>
      //  throw new Exception(s"EmailProperty $p is not supported yet")
      case f: FileProperty =>
        throw new Exception(s"FileProperty $p is not supported yet")
      case o: ObjectProperty =>
        throw new Exception(s"ObjectProperty $p is not supported yet")
      case p: PasswordProperty =>
        throw new Exception(s"PasswordProperty $p is not supported yet")

      case null =>
        throw new Exception("Trying to resolve null property")
      case x =>
        // should not happen as all existing types have been checked before
        throw new Exception(s"Unexpected property type $x")
    }
  }

  def paramType(p: Parameter): Type = {
    if (!p.getRequired)
      OptionClass TYPE_OF noOptParamType(p)
    else
      noOptParamType(p)
  }

  def noOptParamType(p: Parameter): Type = {
    p match {
      case asp: AbstractSerializableParameter[_] =>
        if (asp.getType == "array")
          ListClass TYPE_OF noOptPropType(asp.getItems)
        else
          noOptPropType(PropertyBuilder.build(asp.getType, asp.getFormat, null))
      case bp: BodyParameter =>
        noOptPropType(new RefProperty(bp.getSchema.getReference))
      case rp: RefParameter =>
        RootClass.newClass(rp.getSimpleRef)
    }
  }

  def getProperties(model: Model): Iterable[(String, Property)] = {
    Option(model.getProperties).map(_.asScala).getOrElse(Nil)
  }

}
