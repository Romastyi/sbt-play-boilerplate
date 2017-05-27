package play.boilerplate.generators

import eu.unicredit.swagger.SwaggerConversion
import eu.unicredit.swagger.generators.{ModelGenerator, SyntaxString}
import io.swagger.models.properties.PropertyBuilder
import io.swagger.models.{Model, ModelImpl}
import treehugger.forest._
import definitions._
import treehuggerDSL._
import play.boilerplate.ParserUtils

import scala.collection.JavaConverters._

class PlayModelGenerator extends ModelGenerator with SwaggerConversion {

  def addModelComment(model: Model, tree: Tree): Tree = {
    Option(model.getDescription).map(tree withComment _).getOrElse(tree)
  }

  def generateClass(name: String, model: Model): Seq[Tree] = {

    val trees = model match {
      case EnumerationModel(m, items) =>

        val enum = generateEnumeration(name, items)
        addModelComment(m, enum) :: Nil

      case TypeModel(m, tpe) =>

        val typeAlias = TYPEVAR(name) := TYPE_REF(noOptPropType(PropertyBuilder.build(tpe, null, null)))
        addModelComment(m, typeAlias) :: Nil

      case modelImpl: ModelImpl =>

        val GenClass = RootClass.newClass(name)

        val props = getProperties(modelImpl)

        val params = for ((pname, prop) <- props) yield {
          prop match {
            case EnumProperty(p, _) =>
              PARAM(pname, propType(p, enumerationValueType(name, pname))).empty
            case _ =>
              PARAM(pname, propType(prop.rename(pname))).empty
          }
        }

        val additionalDefs = props.collect {
          case (pname, EnumProperty(_, items)) =>
            generateEnumeration(composeEnumName(name, pname), items)
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

    //println(treeToString(trees: _ *))

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

      val models = Option(swagger.getDefinitions).map(_.asScala).getOrElse(Nil)

      for {
        (name, model) <- models
      } yield SyntaxString(
        name + ".scala",
        generateModelInit(destPackage),
        treeToString(generateClass(name, model): _ *)
      )

    }.getOrElse(Nil)

  }

}
