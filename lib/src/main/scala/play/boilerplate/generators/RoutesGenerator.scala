package play.boilerplate.generators

import eu.unicredit.swagger.StringUtils
import eu.unicredit.swagger.generators.SharedServerClientCode
import io.swagger.models.Swagger
import io.swagger.models.parameters.Parameter
import treehugger.forest._

import scala.collection.JavaConversions._

trait RoutesGenerator
  extends SharedGeneratorCode
    with SharedServerClientCode {

  def generateRoutes(fileName: String, controllerFullName: String): Option[String] = {

    parseSwagger(fileName).flatMap { swagger =>

      val basePath = Option(swagger.getBasePath).getOrElse("/")

      val completePaths = swagger.getPaths.keySet().toSeq

      val routes = completePaths.flatMap(composeRoutes(swagger, controllerFullName, basePath, _))

      if (routes.nonEmpty) {
        Some(routes.mkString("\n", "\n", "\n"))
      } else {
        None
      }

    }

  }

  def composeRoutes(swagger: Swagger, controllerFullName: String, basePath: String, p: String): Seq[String] = {

    Option(swagger.getPath(p)).map { path =>

      for ((httpMethod, op) <- getAllOperations(path).toSeq) yield {

        val url = StringUtils.doUrl(basePath, p)
        val methodName = op.getOperationId
        val methodCall = generateMethodCall(controllerFullName, methodName, op.getParameters)

        s"${StringUtils.padTo(8, httpMethod)}            ${StringUtils.padTo(50, url)}          ${StringUtils.padTo(20, methodCall)}"

      }

    }.getOrElse(
      Nil
    )

  }

  def generateMethodCall(className: String, methodName: String, params: Seq[Parameter]): String = {

    val ps = getMethodParamas(params).map {
      case (n, v) => s"$n: ${treeToString(v.tpt)}"
    }

    s"${generateFullClassName(className)}.$methodName" + ps.mkString("(", ", ", ")")

  }

  def generateFullClassName(className: String): String

}
