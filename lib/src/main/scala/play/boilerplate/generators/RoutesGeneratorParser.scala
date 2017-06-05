package play.boilerplate.generators

import play.boilerplate.parser.model._

trait RoutesGeneratorParser {

  import GeneratorUtils._
  import treehugger.forest._

  def generateRoutes(schema: Schema)(implicit ctx: GeneratorContext): Seq[String] = {

    (for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield composeRoutes(schema.basePath, path, operation)).toIndexedSeq

  }

  def composeRoutes(basePath: String, path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = doRoutesUrl(basePath, path.pathParts, operation)
    val methodCall = generateMethodCall(path, operation)(ctx.addCurrentPath(operation.operationId).setInController(true))

    s"${padTo(8, httpMethod)}            ${padTo(50, url)}          ${padTo(20, methodCall)}"

  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val ps = getMethodParameters(path, operation).map {
      case (n, MethodParam(_, fullQualified, _, _)) => s"$n: ${treeToString(fullQualified.tpt)}"
    }

    generateFullClassName(ctx.controllerClassName) + "." + operation.operationId + ps.mkString("(", ", ", ")")

  }

  def generateFullClassName(className: String): String = "@" + className

}
