package play.boilerplate.generators

import play.boilerplate.parser.model._

abstract class RoutesCodeGenerator(prefix: String) extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val defaultPrefix = prefix + schema.basePath

    val routes = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield composeRoutes(defaultPrefix, path, operation)

    if (routes.nonEmpty) {
      ResourceFile(
        fileName = ctx.settings.routesFileName,
        source = routes.mkString("\n")
      ) :: Nil
    } else {
      Nil
    }

  }

  def composeRoutes(basePath: String, path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = doRoutesUrl(basePath, path.pathParts, operation)
    val methodCall = generateMethodCall(path, operation)(ctx.addCurrentPath(operation.operationId).setInClient(true))

    s"${padTo(8, httpMethod)}            ${padTo(50, url)}          ${padTo(20, methodCall)}"

  }

  def doRoutesUrl(basePath: String, path: Iterable[PathPart], operation: Operation): String = {

    val p1 = if (basePath.startsWith("/")) basePath else "/" + basePath
    val p2 = if (p1.endsWith("/")) p1.dropRight(1) else p1

    val parts = path.collect {
      case StaticPart(str) =>
        str
      case ParamPart(name) =>
        val param = operation.parameters.find(_.name == name).map(_.baseDef).getOrElse {
          throw new RuntimeException(s"Url path parameter '$name' not found for operation (${operation.operationId}).")
        }
        param match {
          case _: IntegerDefinition | _: LongDefinition => "$" + name + "<[0-9]+>"
          case s: StringDefinition if s.pattern.isDefined => "$" + name + "<" + s.pattern.get + ">"
          case _ => ":" + name
        }
    }.toSeq

    cleanDuplicateSlash((p2 +: parts).mkString("/"))

  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): String = {

    val ps = getMethodParameters(path, operation, withHeaders = false).map {
      case (n, MethodParam(_, fullQualified, _, _, defaultValue, _)) =>
        s"$n: ${treeToString(fullQualified.tpt)}" + defaultValue.map(
          literal => " ?= " + treeToString(literal)
        ).getOrElse("")
    }

    val fullControllerClassName = composeName(ctx.settings.controllerPackageName, ctx.settings.controllerClassName)

    generateFullClassName(fullControllerClassName) + "." + operation.operationId + ps.mkString("(", ", ", ")")

  }

  def generateFullClassName(className: String): String

}

final case class DynamicRoutesCodeGenerator(prefix: String = "/") extends RoutesCodeGenerator(prefix) {
  override def generateFullClassName(className: String): String = "@" + className
}

final case class InjectedRoutesCodeGenerator(prefix: String = "/") extends RoutesCodeGenerator(prefix) {
  override def generateFullClassName(className: String): String = className
}

final case class SingletonRoutesCodeGenerator(implSuffix: String = "Impl", prefix: String = "/") extends RoutesCodeGenerator(prefix) {
  override def generateFullClassName(className: String): String = className + implSuffix
}
