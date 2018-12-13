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
      IMPORT(REF("play.boilerplate.api.server.dsl.SirdOps"), "_")/*,
      IMPORT(REF(ctx.settings.controllerClassName), "_")*/
    ) ++
      ctx.settings.injectionProvider.imports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
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

    val routesCases = (for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield composeRoutesCases(defaultPrefix, path, operation)).toIndexedSeq

    val definitions = filterNonEmptyTree(routesCases.flatMap(_.definitions))
    val routes = filterNonEmptyTree(routesCases.map(_.caseDef))

    if (routes.nonEmpty) {

      val imports = BLOCK {
        generateImports
      } inPackage ctx.settings.controllerPackageName

      val routesMethod = DEF("routes", TYPE_REF("Router.Routes")).withFlags(Flags.OVERRIDE) := BLOCK {
        routes
      }

      val classDef = CLASSDEF(routerClassName)
        .withParents("SimpleRouter") :=
        BLOCK(
          definitions :+ routesMethod
        )

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

  private case class RoutesCase(definitions: Seq[Tree], caseDef: CaseDef)

  private def composeRoutesCases(basePath: String, path: Path, operation: Operation)
                                (implicit ctx: GeneratorContext): RoutesCase = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = composeRoutesUrl(basePath, path.pathParts, operation)
    val queryInterp = composeQueryParams(path, operation)
    val queryParams = queryInterp.map(_.interp)
    val queryDefs   = filterNonEmptyTree(queryInterp.map(_.definition))
    val methodCall = generateMethodCall(path, operation)

    val fullUrl = INFIX_CHAIN("&", url +: queryParams)

    RoutesCase(
      definitions = queryDefs,
      caseDef = CASE(REF(httpMethod) UNAPPLY fullUrl) ==> methodCall
    )

  }

  private def paramInterp(name: String, param: Definition): String = param match {
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

  private case class QueryInterp(definition: Tree, interp: Interpolated)

  @tailrec
  private def queryParamInterpSym(param: Definition)(implicit ctx: GeneratorContext): QueryInterp = {

    def interpolated(symbol: String, paramInterp: String): Interpolated = {
      INTERP(symbol, LIT(param.name + "=" + paramInterp))
    }

    param match {
      case RefDefinition(_, ref) =>
        queryParamInterpSym(ref)
      case _: OptionDefinition =>
        QueryInterp(EmptyTree, interpolated("q_?", paramInterp(param.name, param.baseDef)))
      case a: ArrayDefinition
        if a.collectionFormat == CollectionFormat.Multi || a.collectionFormat == CollectionFormat.None =>
        QueryInterp(EmptyTree, interpolated("q_*", paramInterp(param.name, param.baseDef)))
      case a: ArrayDefinition =>
        val name = param.name
        val sep = a.collectionFormat match {
          case CollectionFormat.Csv => ','
          case CollectionFormat.Tsv => '\t'
          case CollectionFormat.Ssv => ' '
          case CollectionFormat.Pipes => '|'
          case other => throw new RuntimeException(s"Unsupported 'collectionFormat' ($other) for array parameter ($name).")
        }
        val tpe = getTypeSupport(a.baseDef).tpe
        QueryInterp(
          VAL(s"interp_$name") := REF("listOf") APPLYTYPE tpe APPLY LIT(sep),
          interpolated("q", "${interp_" + name + "(" + name + ")}")
        )
      case _ =>
        QueryInterp(EmptyTree, interpolated("q", paramInterp(param.name, param)))
    }

  }

  private def composeQueryParams(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Seq[QueryInterp] = {
    (path.parameters ++ operation.parameters).toIndexedSeq.collect {
      case param: QueryParameter => queryParamInterpSym(param)
    }
  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Tree = {

    val ps = getMethodParameters(path, operation, withHeaders = false).map {
      case (name, _) => ID(name)
    }

    REF("controller") DOT operation.operationId APPLY (ps: _ *)

  }

}
