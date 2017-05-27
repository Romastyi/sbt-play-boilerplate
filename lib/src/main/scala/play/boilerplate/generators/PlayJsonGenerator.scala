package play.boilerplate.generators

import eu.unicredit.swagger.SwaggerConversion
import eu.unicredit.swagger.generators.{JsonGenerator, SyntaxString}
import io.swagger.models.{Model, ModelImpl}
import treehugger.forest._
import definitions._
import treehuggerDSL._
import play.boilerplate.ParserUtils

import scala.collection.JavaConverters._

class PlayJsonGenerator extends JsonGenerator with SwaggerConversion {

  def generateJsonInit(packageName: String): String = {
    val initTree =
      BLOCK {
        Seq(IMPORT("play.api.libs.json", "_"))
      } inPackage packageName

    treeToString(initTree)
  }

  def generateJsonRW(fileName: String): Iterable[ValDef] = {

    ParserUtils.parseSwagger(fileName).map { swagger =>

      val models = Option(swagger.getDefinitions).map(_.asScala).getOrElse(Nil)

      val trees = for {
        (name, model) <- models
      } yield generateJsonModelRW(name, model)

      trees.flatten

    }.getOrElse(Nil)

  }

  def generateJsonModelRW(name: String, model: Model): Iterable[ValDef] = {

    model match {
      case EnumerationModel(_, _) =>
        generateEnumReads(name) :: generateEnumWrites(name) :: Nil
      case TypeModel(_, _) =>
        Nil
      case modelImpl: ModelImpl =>
        generateModelReads(name, modelImpl) ++ generateModelWrites(name, modelImpl)
      case _ =>
        throw new Exception(s"Unsupported model type $model")
    }

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

  def generateModelReads(modelName: String, model: ModelImpl): Iterable[ValDef] = {

    val properties = getProperties(model)
    val caseObject = properties.isEmpty
    val modelType = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)
    val readsType = definitions.getClass("Reads") TYPE_OF modelType

    val modelReads = VAL(s"${modelName}Reads", readsType) withFlags (Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(readsType) :=
        LAMBDA(PARAM("json").tree) ==>
          REF("JsSuccess") APPLY {
          if (caseObject) {
            REF(modelName)
          } else {
            REF(modelName) APPLY {
              for ((pname, prop) <- properties) yield {
                val mtd = if (!prop.getRequired) "asOpt" else "as"
                PAREN(REF("json") INFIX ("\\", LIT(pname))) DOT mtd APPLYTYPE {
                  prop match {
                    case EnumProperty(_, _) => enumerationValueType(modelName, pname)
                    case _ => noOptPropType(prop.rename(pname))
                  }
                }
              }
            }
          }
        }
    }

    val additionalReads = properties.collect {
      case (pname, EnumProperty(_, _)) =>
        generateEnumReads(composeEnumName(modelName, pname))
    }

    additionalReads.toSeq :+ modelReads

  }

  def generateModelWrites(modelName: String, model: ModelImpl): Iterable[ValDef] = {

    val properties = getProperties(model)
    val caseObject = properties.isEmpty
    val typeName = if (caseObject) TYPE_SINGLETON(TYPE_REF(modelName)) else TYPE_REF(modelName)
    val writesType = definitions.getClass("Writes") TYPE_OF typeName

    val modelWrites = VAL(s"${modelName}Writes", writesType) withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      ANONDEF(writesType) :=
        LAMBDA(PARAM("o").tree) ==>
          REF("JsObject") APPLY {
          if (caseObject) {
            SeqClass APPLY Seq.empty
          } else {
            SeqClass APPLY {
              for ((pname, prop) <- properties) yield {
                LIT(pname) INFIX ("->", (REF("Json") DOT "toJson")(REF("o") DOT pname))
              }
            } DOT "filter" APPLY (REF("_") DOT "_2" INFIX ("!=", REF("JsNull")))
          }
        }
    }

    val additionalWrites = properties.collect {
      case (pname, EnumProperty(_, _)) =>
        generateEnumWrites(composeEnumName(modelName, pname))
    }

    additionalWrites.toSeq :+ modelWrites

  }

  def generateJson(destPackage: String, vds: Iterable[ValDef]): Iterable[SyntaxString] = {

    if (vds.nonEmpty) {

      val pre = generateJsonInit(destPackage)

      val tree = PACKAGEOBJECTDEF("json") := BLOCK(vds)

      val code = treeToString(tree)

      //println(code)

      Seq(SyntaxString("json", pre, code))

    } else {

      Nil

    }

  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {
    generateJson(destPackage, generateJsonRW(fileName))
  }

}
