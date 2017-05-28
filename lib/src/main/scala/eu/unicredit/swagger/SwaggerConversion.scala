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

import io.swagger.models._
import io.swagger.models.properties._
import io.swagger.models.parameters._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.annotation.tailrec
import scala.collection.JavaConverters._

trait SwaggerConversion {

  def getDefinitions(swagger: Swagger): Map[String, Model] = {
    Option(swagger.getDefinitions).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  object ArrayModel {
    def unapply(arg: Model): Option[(ArrayModel, Property)] = {
      arg match {
        case model: ArrayModel =>
          Some((model, model.getItems))
        case _ =>
          None
      }
    }
  }

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

  object EnumModel {
    def unapply(arg: Model): Option[(ModelImpl, Iterable[String])] = {
      arg match {
        case TypeModel(modelImpl, StringProperty.TYPE) if Option(modelImpl.getEnum).isDefined =>
          Some((modelImpl, modelImpl.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  object ObjectModel {
    def unapply(arg: Model): Option[ModelImpl] = {
      arg match {
        case model: ModelImpl if Option(model.getType).isEmpty || model.getType == ObjectProperty.TYPE =>
          Some(model)
        case _ =>
          None
      }
    }
  }

  def getProperties(model: Model): Iterable[(String, Property)] = {
    Option(model.getProperties).map(_.asScala).getOrElse(Nil)
  }

  def propType(p: Property, models: Map[String, Model] = Map.empty): PropType = {
    val PropType(tpe, d) = noOptPropType(p, models)
    PropType(propType(p, tpe), d)
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

  private lazy val ExceptionClass =
    definitions.getClass("Exception")

  private lazy val OffsetDateTimeClass =
    definitions.getClass("java.time.OffsetDateTime")

  private lazy val LocalDateClass =
    definitions.getClass("java.time.LocalDate")

  private lazy val UUIDClass =
    definitions.getClass("java.util.UUID")

  private lazy val EnumerationClass =
    definitions.getClass("Enumeration")

  def composeEnumName(modelName: String, fieldName: String): String = {
    StringUtils.toCamelCase(modelName) + StringUtils.toCamelCase(fieldName)
  }

  def enumerationValueType(enum: Symbol): Type = {
    TYPE_REF(enum DOT "Value")
  }

  def enumerationValueType(modelName: String, fieldName: String): Type = {
    enumerationValueType(definitions.getClass(composeEnumName(modelName, fieldName)))
  }

  def generateEnumeration(name: String, items: Iterable[String]): ImplDef = {

    OBJECTDEF(RootClass.newClass(name)) withParents EnumerationClass := BLOCK {
      items.map { item =>
        VAL(StringUtils.toCamelCase(item)) := REF("Value") APPLY LIT(item)
      }
    }

  }

  def generateEnumeration(modelName: String, fieldName: String, items: Iterable[String]): ImplDef = {
    generateEnumeration(composeEnumName(modelName, fieldName), items)
  }

  def generateEnumReads(enumName: String): ValDef = {

    val enumType = definitions.getClass(enumName)
    val enumValueType = enumerationValueType(enumType)
    val readsType = definitions.getClass("Reads") TYPE_OF enumValueType

    VAL(s"${enumName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(readsType) := BLOCK (
        CASE(REF("JsString") UNAPPLY ID("name")) ==> {
          TRY {
            REF("JsSuccess") APPLY (enumType DOT "withName" APPLY REF("name"))
          } CATCH {
            CASE(WILDCARD withType TYPE_REF("NoSuchElementException")) ==> {
              REF("JsError") APPLY INFIX_CHAIN("+",
                LIT("Enumeration expected of type: '"),
                enumType DOT "getClass",
                LIT("', but it does not appear to contain the value: '"),
                REF("name"),
                LIT("'.")
              )
            }
          } ENDTRY
        },
        CASE(WILDCARD) ==> {
          REF("JsError") APPLY INFIX_CHAIN("+",
            LIT("Enumeration expected of type: '"),
            enumType DOT "getClass",
            LIT("'.")
          )
        }
      )
    }

  }

  def generateEnumWrites(enumName: String): ValDef = {

    val enumType = definitions.getClass(enumName)
    val enumValueType = enumerationValueType(enumType)
    val writesType = definitions.getClass("Writes") TYPE_OF enumValueType

    VAL(s"${enumName}Writes", writesType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(writesType) :=
        LAMBDA(PARAM("value").tree) ==>
          REF("JsString") APPLY (REF("value") DOT "toString")
    }

  }

  def generateEnumQueryBindable(modelName: String, fieldName: String): Tree = {
    generateEnumQueryBindable(composeEnumName(modelName, fieldName))
  }

  def generateEnumQueryBindable(enumName: String): Tree = {

    val enumType = definitions.getClass(enumName)
    val enumValue = enumerationValueType(enumType)

    val queryBindable = (TYPE_REF("QueryStringBindable") DOT "Parsing") APPLYTYPE enumValue APPLY(
      enumType DOT "withName",
      WILDCARD DOT "toString",
      LAMBDA(PARAM("key", StringClass).tree, PARAM("e", ExceptionClass).tree) ==>
        LIT(s"Cannot parse parameter %s as ${enumValue.toString()}: %s") DOT "format"
        APPLY(REF("key"), REF("e") DOT "getMessage")
    )

    OBJECTDEF(s"${enumName}Query").withParents(queryBindable).withFlags(Flags.IMPLICIT).tree

  }

  def generateEnumDefs(modelName: String, fieldName: String, items: Iterable[String]): Seq[PropDefs] = {
    generateEnumDefs(composeEnumName(modelName, fieldName), items)
  }

  def generateEnumDefs(name: String, items: Iterable[String]): Seq[PropDefs] = {
    PropDefs(
      definition = generateEnumeration(name, items),
      jsonReads  = generateEnumReads(name),
      jsonWrites = generateEnumWrites(name),
      queryBindable = generateEnumQueryBindable(name)
    ) :: Nil
  }

  @tailrec
  final def findTypeByRef(ref: String, models: Map[String, Model]): PropType = {
    models.get(ref) match {
      case Some(EnumModel(_, items)) =>
        val defs = generateEnumDefs(ref, items).map(_.copy(definition =  EmptyTree))
        PropType(enumerationValueType(definitions.getClass(ref)), defs)
      case Some(ArrayModel(_, prop)) =>
        val PropType(tpe, d) = noOptPropType(prop, models)
        PropType(ListClass TYPE_OF tpe, d)
      case Some(r : RefModel) =>
        findTypeByRef(r.getSimpleRef, models)
      case _ =>
        PropType(definitions.getClass(ref))
    }
  }

  case class PropDefs(definition: Tree, jsonReads: Tree, jsonWrites: Tree, queryBindable: Tree)
  case class PropType(tpe: Type, additionalDef: Seq[PropDefs] = Nil)

  def noOptPropType(p: Property, models: Map[String, Model] = Map.empty): PropType = {
    p match {
      case EnumProperty(_, _) =>
        throw new Exception(s"Enums are not supported yet")
      case _: StringProperty =>
        PropType(StringClass)
      case _: BooleanProperty =>
        PropType(BooleanClass)
      case _: DoubleProperty =>
        PropType(DoubleClass)
      case _: FloatProperty =>
        PropType(FloatClass)
      case _: IntegerProperty =>
        PropType(IntClass)
      case _: LongProperty =>
        PropType(LongClass)
      case _: BaseIntegerProperty =>
        PropType(IntClass)
      case _: DecimalProperty =>
        PropType(BigDecimalClass)
      case _: DateProperty =>
        PropType(LocalDateClass)
      case _: DateTimeProperty =>
        PropType(OffsetDateTimeClass)
      case _: UUIDProperty =>
        PropType(UUIDClass)
      case m: MapProperty =>
        val PropType(tpe, d) = noOptPropType(m.getAdditionalProperties, models)
        PropType(RootClass.newClass("Map") TYPE_OF (StringClass, tpe), d)
      case a: ArrayProperty =>
        val PropType(tpe, d) = noOptPropType(a.getItems, models)
        PropType(ListClass TYPE_OF tpe, d)
      case r: RefProperty =>
        findTypeByRef(r.getSimpleRef, models)

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

  object TypeParameter {
    def unapply(arg: Parameter): Option[AbstractSerializableParameter[_]] = {
      arg match {
        case param: AbstractSerializableParameter[_] =>
          Some(param)
        case _ =>
          None
      }
    }
  }

  object EnumParameter {
    def unapply(arg: Parameter): Option[(AbstractSerializableParameter[_], Iterable[String])] = {
      arg match {
        case TypeParameter(param) if Option(param.getEnum).isDefined =>
          Some((param, param.getEnum.asScala))
        case _ =>
          None
      }
    }
  }

  object ArrayParameter {
    def unapply(arg: Parameter): Option[(AbstractSerializableParameter[_], Property)] = {
      arg match {
        case TypeParameter(param) if param.getType == ArrayProperty.TYPE =>
          Some((param, param.getItems))
        case _ =>
          None
      }
    }
  }

  object BodyParameter {
    def unapply(arg: Parameter): Option[(BodyParameter, Model)] = {
      arg match {
        case param: BodyParameter =>
          Some((param, param.getSchema))
        case _ =>
          None
      }
    }
  }

  def paramType(p: Parameter, models: Map[String, Model] = Map.empty): PropType = {
    val PropType(tpe, d) = noOptParamType(p, models)
    PropType(paramType(p, tpe), d)
  }

  def paramType(p: Parameter, tpe: Type): Type = {
    if (Option(p.getRequired).getOrElse(false))
      tpe
    else
      OptionClass TYPE_OF tpe
  }

  def noOptParamType(p: Parameter, models: Map[String, Model] = Map.empty): PropType = {
    p match {
      case ArrayParameter(_, items) =>
        val PropType(tpe, d) = noOptPropType(items, models)
        PropType(ListClass TYPE_OF tpe, d)
      case TypeParameter(param) =>
        val prop = PropertyBuilder.build(param.getType, param.getFormat, null)
        noOptPropType(prop, models)
      case BodyParameter(_, ArrayModel(_, items)) =>
        val PropType(tpe, d) = noOptPropType(items, models)
        PropType(ListClass TYPE_OF tpe, d)
      case BodyParameter(_, model) =>
        noOptPropType(new RefProperty(model.getReference), models)
      case rp: RefParameter =>
        findTypeByRef(rp.getSimpleRef, models)
    }
  }

  def fullParamType(className: String, p: Parameter, models: Map[String, Model]): PropType = {
    val PropType(tpe, d) = fullNoOptParamType(className, p, models)
    PropType(paramType(p, tpe), d)
  }

  def fullNoOptParamType(className: String, p: Parameter, models: Map[String, Model]): PropType = {
    p match {
      case ArrayParameter(_, EnumProperty(_, items)) =>
        val tpe = enumerationValueType(className, p.getName)
        val defs = generateEnumDefs(className, p.getName, items)
        PropType(ListClass TYPE_OF tpe, defs)
      case EnumParameter(_, items) =>
        val tpe = enumerationValueType(className, p.getName)
        val defs = generateEnumDefs(className, p.getName, items)
        PropType(tpe, defs)
      case param =>
        noOptParamType(param, models)
    }
  }

}
