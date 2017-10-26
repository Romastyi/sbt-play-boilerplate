package play.boilerplate.generators

import play.boilerplate.parser.model._

trait RoutesCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val routes = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield composeRoutes(schema.basePath, path, operation)

    ResourceFile(
      fileName = ctx.settings.routesFileName,
      source = routes.mkString("\n")
    ) :: Nil

  }

  def composeRoutes(basePath: String, path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = doRoutesUrl(basePath, path.pathParts, operation)
    val methodCall = generateMethodCall(path, operation)(ctx.addCurrentPath(operation.operationId).setInClient(true))

    s"${padTo(8, httpMethod)}            ${padTo(50, url)}          ${padTo(20, methodCall)}"

  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val ps = getMethodParameters(path, operation).map {
      case (n, MethodParam(_, fullQualified, _, _, defaultValue)) =>
        s"$n: ${treeToString(fullQualified.tpt)}" + defaultValue.map(
          literal => " ?= " + treeToString(literal)
        ).getOrElse("")
    }

    val fullControllerClassName = composeName(ctx.settings.controllerPackageName, ctx.settings.controllerClassName)

    generateFullClassName(fullControllerClassName) + "." + operation.operationId + ps.mkString("(", ", ", ")")

  }

  def generateFullClassName(className: String): String

}
