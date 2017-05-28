package play.boilerplate.generators

import eu.unicredit.swagger.SwaggerConversion
import eu.unicredit.swagger.generators.{ModelGenerator, SyntaxString}
import io.swagger.models.properties.PropertyBuilder
import io.swagger.models.Model
import treehugger.forest._
import definitions._
import treehuggerDSL._
import play.boilerplate.ParserUtils

class PlayModelGenerator extends ModelGenerator with SwaggerConversion {

  def addModelComment(model: Model, tree: Tree): Tree = {
    Option(model.getDescription).map(tree withComment _).getOrElse(tree)
  }

  def generateClass(name: String, model: Model, others: Map[String, Model]): Seq[Tree] = {

    val trees = model match {
      case EnumModel(m, items) =>

        val enum = generateEnumeration(name, items)
        addModelComment(m, enum) :: Nil

      case TypeModel(m, tpe) =>

        val typeAlias = TYPEVAR(name) := TYPE_REF(noOptPropType(PropertyBuilder.build(tpe, null, null)).tpe)
        addModelComment(m, typeAlias) :: Nil

      case ObjectModel(modelImpl) =>

        val GenClass = RootClass.newClass(name)

        val props = getProperties(modelImpl)

        val params = for ((pname, prop) <- props) yield {
          prop match {
            case EnumProperty(p, _) =>
              PARAM(pname, propType(p, enumerationValueType(name, pname))).empty
            case _ =>
              PARAM(pname, propType(prop.rename(pname), others).tpe).empty
          }
        }

        val additionalDefs = props.collect {
          case (pname, EnumProperty(_, items)) =>
            generateEnumeration(name, pname, items)
        }

        val modelDef = if (params.isEmpty) {
          CASEOBJECTDEF(GenClass).tree
        } else {
          CASECLASSDEF(GenClass).withParams(params).tree
        }

        additionalDefs.toSeq :+ modelDef

      case _ =>
        throw new Exception(s"Unsupported model type $model")
    }

    trees

  }

  def generateModelInit(packageName: String): String = {
    //val initTree =
    //PACKAGE(packageName)

    //treeToString(initTree)
    "package " + packageName
  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {

    ParserUtils.parseSwagger(fileName).map { swagger =>

      val models = getDefinitions(swagger)

      for {
        (name, model) <- models
      } yield SyntaxString(
        name + ".scala",
        generateModelInit(destPackage),
        treeToString(generateClass(name, model, models): _ *)
      )

    }.getOrElse(Nil)

  }

}
