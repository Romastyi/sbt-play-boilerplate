package play.boilerplate.generators

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._

import scala.annotation.tailrec

case class SirdRouterGenerator(prefix: String = "/") extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(REF("play.api.routing"), "Router", "SimpleRouter"),
      IMPORT(REF("play.api.routing.sird"), "_"),
      IMPORT(REF(ctx.settings.controllerClassName), "_")
    )
  }

  def dependencies(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(
      InjectionProvider.Dependency("controller", TYPE_REF(ctx.settings.controllerClassName))
    )
  }

  def routerClassName(implicit ctx: GeneratorContext): String = {
    objectNameFromFileName(ctx.settings.fileName, "Router")
  }

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val defaultPrefix = prefix + schema.basePath

    val routes = filterNonEmptyTree(
      (for {
        path <- schema.paths
        (_, operation) <- path.operations.toSeq.sortBy(_._1)
      } yield composeRoutes(defaultPrefix, path, operation)).toIndexedSeq
    )

    if (routes.nonEmpty) {

      val imports = BLOCK {
        generateImports
      } inPackage ctx.settings.controllerPackageName

      val classDef = CLASSDEF(routerClassName)
        .withParents("SimpleRouter") :=
        BLOCK {
          DEF("routes", TYPE_REF("Router.Routes")).withFlags(Flags.OVERRIDE) := BLOCK {
            routes
          }
        }

      // --- DI
      val routerTree = ctx.settings.injectionProvider.classDefModifier(classDef, dependencies)

      SourceCodeFile(
        packageName = ctx.settings.controllerPackageName,
        className = routerClassName,
        header = treeToString(imports),
        impl = routerTree
      ) :: Nil

    } else {
      Nil
    }

  }

  def composeRoutes(basePath: String, path: Path, operation: Operation)(implicit ctx: GeneratorContext): CaseDef = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = composeRoutesUrl(basePath, path.pathParts, operation)
    val queryParams = composeQueryParams(path, operation)
    val methodCall = generateMethodCall(path, operation)

    val fullUrl = INFIX_CHAIN("&", url +: queryParams)

    CASE(REF(httpMethod) UNAPPLY fullUrl) ==> methodCall

  }

  protected def paramInterp(name: String, param: Definition): String = param match {
    case _: IntegerDefinition => "${int(" + name + ")}"
    case _: LongDefinition => "${long(" + name + ")}"
    case _: FloatDefinition => "${float(" + name + ")}"
    case _: DoubleDefinition => "${double(" + name + ")}"
    case _: BooleanDefinition => "${bool(" + name + ")}"
    case _ => "$" + name
  }

  protected def composeRoutesUrl(basePath: String, path: Iterable[PathPart], operation: Operation)
                                (implicit ctx: GeneratorContext): Interpolated = {

    val p1 = if (basePath.startsWith("/")) basePath else "/" + basePath
    val p2 = if (p1.endsWith("/")) p1.dropRight(1) else p1

    val parts = path.collect {
      case StaticPart(str) =>
        str
      case ParamPart(name) =>
        val param = operation.parameters.find(_.name == name).map(_.baseDef).getOrElse {
          throw new RuntimeException(s"Url path parameter '$name' not found for operation (${operation.operationId}).")
        }
        paramInterp(name, param)
    }.toSeq

    INTERP("p", LIT(cleanDuplicateSlash((p2 +: parts).mkString("/"))))

  }

  @tailrec
  final def queryParamInterpSym(param: Definition): String = param match {
    case RefDefinition(_, ref) => queryParamInterpSym(ref)
    case _: OptionDefinition => "q_?"
    case _: ArrayDefinition => "q_*"
    case _ => "q"
  }

  protected def composeQueryParams(path: Path, operation: Operation)
                                  (implicit ctx: GeneratorContext): Seq[Interpolated] = {
    (path.parameters ++ operation.parameters).toSeq.collect {
      case param: QueryParameter =>
        val interpSym = queryParamInterpSym(param)
        val interp = paramInterp(param.name, param.baseDef)
        INTERP(interpSym, LIT(param.name + "=" + interp))
    }
  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Tree = {

    val ps = getMethodParameters(path, operation, withHeaders = false).keys.map {
      name => ID(name)
    }.toIndexedSeq

    REF("controller") DOT operation.operationId APPLY (ps: _ *)

  }

}
