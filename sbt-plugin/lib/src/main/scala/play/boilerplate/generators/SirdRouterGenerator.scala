package play.boilerplate.generators

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._

case class SirdRouterGenerator(prefix: String = "/") extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF(ctx.settings.servicePackageName), ctx.settings.serviceClassName),
      IMPORT(REF(ctx.settings.serviceClassName), "_"),
      IMPORT(REF("play.api.routing"), "Router", "SimpleRouter"),
      IMPORT(REF("play.api.routing.sird"), "_"),
      IMPORT(REF("play.boilerplate.api.server.dsl.SirdOps"), "_")
    ) ++
      ctx.settings.injectionProvider.imports ++
      ctx.settings.codeProvidedPackages.filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
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
    } yield composeRoutesCases(defaultPrefix, path, operation)(ctx.addCurrentPath(operation.operationId).setInController(true))).toIndexedSeq

    val companionDefs = distinctTreeByName(routesCases.flatMap(_.companionDefs))
    val definitions = filterNonEmptyTree(routesCases.flatMap(_.definitions))
    val routes = filterNonEmptyTree(routesCases.map(_.commentedCase))

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
          IMPORT(REF(routerClassName), "_") +: definitions :+ routesMethod
        )

      val companionObj = OBJECTDEF(routerClassName) := {
        if (companionDefs.nonEmpty) {
          val needController = routesCases.exists(_.needController)
          val importController = Seq(IMPORT(REF(ctx.settings.controllerClassName), "_")).filter(_ => needController)
          BLOCK(
            importController ++ companionDefs
          )
        } else EmptyTree
      }

      // --- DI
      val routerTree = ctx.settings.injectionProvider.classDefModifier(classDef, dependencies)

      SourceCodeFile(
        packageName = ctx.settings.controllerPackageName,
        className = routerClassName,
        header = treeToString(imports),
        impl = routerTree + "\n\n" + treeToString(companionObj)
      ) :: Nil

    } else {
      Nil
    }

  }

  private case class RoutesCase(definitions: Seq[Tree], companionDefs: Seq[Tree], caseDef: CaseDef, needController: Boolean, comment: String) {
    def commentedCase: Commented = caseDef.withComment(comment)
  }

  private def composeRoutesCases(basePath: String, path: Path, operation: Operation)
                                (implicit ctx: GeneratorContext): RoutesCase = {

    val httpMethod = operation.httpMethod.toString.toUpperCase
    val url = composeRoutesUrl(basePath, path.pathParts, operation)
    val queryInterp = composeQueryParams(path, operation)
    val queryParams = queryInterp.map(_.interp)
    val queryDefs   = filterNonEmptyTree(queryInterp.map(_.definition))
    val methodCall  = generateMethodCall(path, operation)
    val companionExtractors = url.extractors ++ queryInterp.flatMap(_.extractor)
    val companionDefs = filterNonEmptyTree(companionExtractors.map(_.definition))

    val fullUrl = INFIX_CHAIN("&", url.interp +: queryParams)

    RoutesCase(
      definitions = queryDefs,
      companionDefs = companionDefs,
      caseDef = CASE(REF(httpMethod) UNAPPLY fullUrl) ==> methodCall,
      needController = companionExtractors.exists(_.needController),
      comment = RoutesCodeGenerator.composeRoutes(prefix, path, operation, identity).prettyPrint(4,0)
    )

  }

  private case class ParamExtractor(definition: Tree, name: String, needController: Boolean)

  private def getParamExtractor(param: Parameter, methodCall: String, extractorBaseType: String, f: (String, Type, TypeApply) => Tree)
                               (implicit ctx: GeneratorContext): Option[ParamExtractor] = {

    val (_, methodParam) = getMethodParam(param)
    val needOption = methodParam.defaultValue.nonEmpty && isOptional(param)

    if (filterNonEmptyTree(methodParam.implicits).nonEmpty) {
      val valueType = if (needOption) TYPE_OPTION(getTypeSupport(param.ref).tpe) else getTypeSupport(param.ref).tpe
      val baseSupport = getTypeSupport(param.baseDef)
      val extractorType = RootClass.newClass(extractorBaseType) TYPE_OF valueType
      val extractorCont = (if (needOption) Seq("OptionOf") else Nil) ++ collectTypeContainers(param)
      val extractorName = methodCall + extractorCont.mkString("") + stringToValidIdentifier(baseSupport.tpe.safeToString, skipNotValidChars = true)
      Some(ParamExtractor(f(extractorName, extractorType, REF(methodCall) APPLYTYPE valueType), extractorName, needController = true))
    } else {
      None
    }

  }

  private def getPathParamExtractor(param: PathParameter)(implicit ctx: GeneratorContext): Option[ParamExtractor] = {
    getParamExtractor(param, "pathOf", "PathBindableExtractor", {
      case (extractorName, extractorType, methodCall) =>
        VAL(extractorName, extractorType) := methodCall
    })
  }

  private def getExtendedPathParamExtractor(operationId: String, param: PathParameter)(implicit ctx: GeneratorContext): Option[ParamExtractor] = {
    param.baseDef match {
      case s: StringDefinition => s.pattern.map { pattern =>
        val extractorType = RootClass.newClass("PathBindableExtractor") TYPE_OF StringClass
        val extractorName = operationId + "PathOf" + stringToValidIdentifier(param.name.capitalize, skipNotValidChars = true)
        ParamExtractor(VAL(extractorName, extractorType) := REF("pathOfRx") APPLY LIT(pattern.replaceAllLiterally("$", "$$")), extractorName, needController = false)
      }
      case _ =>
        None
    }
  }

  private def getQueryParamExtractor(param: QueryParameter)(implicit ctx: GeneratorContext): Option[ParamExtractor] = {
    getParamExtractor(param, "queryOf", "QueryStringParameterExtractor", {
      case (extractorName, extractorType, methodCall) =>
        DEF(extractorName, extractorType).withParams(PARAM("paramName", StringClass).tree) := methodCall APPLY REF("paramName")
    })
  }

  private def getExtendedQueryParamExtractor(operationId: String, param: QueryParameter)(implicit ctx: GeneratorContext): Option[ParamExtractor] = {
    param.baseDef match {
      case s: StringDefinition => s.pattern.map { pattern =>
        val extractorType = RootClass.newClass("QueryStringParameterExtractor") TYPE_OF StringClass
        val extractorName = operationId + "QueryOf" + stringToValidIdentifier(param.name.capitalize, skipNotValidChars = true)
        ParamExtractor(DEF(extractorName, extractorType).withParams(PARAM("paramName", StringClass).tree) := REF("queryOfRx") APPLY (REF("paramName"), LIT(pattern.replaceAllLiterally("$", "$$"))), extractorName, needController = false)
      }
      case _ =>
        None
    }
  }

  private def paramBaseInterp(name: String, param: Definition): String = param match {
    case _: IntegerDefinition => "${int(" + name + ")}"
    case _: LongDefinition => "${long(" + name + ")}"
    case _: FloatDefinition => "${float(" + name + ")}"
    case _: DoubleDefinition => "${double(" + name + ")}"
    case _: BooleanDefinition => "${bool(" + name + ")}"
    case _: StringDefinition => "$" + name// + s.pattern.map(p => s"<${p.replaceAllLiterally("$", "$$")}>").getOrElse("")
    case _ => "$" + name
  }

  private case class PathInterp(interp: Interpolated, extractors: Seq[ParamExtractor])

  private def composeRoutesUrl(basePath: String, path: Iterable[PathPart], operation: Operation)
                              (implicit ctx: GeneratorContext): PathInterp = {

    val p1 = if (basePath.startsWith("/")) basePath else "/" + basePath
    val p2 = if (p1.endsWith("/")) p1.dropRight(1) else p1

    case class PathPart(str: String, extractor: Option[ParamExtractor])

    val parts = path.collect {
      case StaticPart(str) =>
        PathPart(str, None)
      case ParamPart(name) =>
        val param = operation.parameters.collectFirst {
          case path: PathParameter if path.name == name => path
        }.getOrElse {
          throw new RuntimeException(s"Url path parameter '$name' not found for operation (${operation.operationId}).")
        }

        getPathParamExtractor(param) orElse getExtendedPathParamExtractor(operation.operationId, param) match {
          case Some(extractor @ ParamExtractor(_ , extractorName, _)) =>
            PathPart("${" + extractorName + "(" + name + ")}", Some(extractor))
          case None =>
            PathPart(paramBaseInterp(name, param.baseDef), None)
        }
    }.toSeq

    PathInterp(
      interp = INTERP("p", LIT(cleanDuplicateSlash((p2 +: parts.map(_.str)).mkString("/")))),
      extractors = parts.flatMap(_.extractor)
    )

  }

  private case class QueryInterp(definition: Tree, interp: Tree, extractor: Option[ParamExtractor])

  private def queryParamInterpSym(param: QueryParameter, operationId: String, idx: Int)(implicit ctx: GeneratorContext): QueryInterp = {

    def interpolated(symbol: String, paramInterp: String): Interpolated = {
      INTERP(symbol, LIT(param.name + "=" + paramInterp))
    }

    val paramName = param.name

    getQueryParamExtractor(param) orElse getExtendedQueryParamExtractor(operationId, param) match {
      case Some(extractor @ ParamExtractor(_, extractorName, _)) =>
        val valName = operationId + "Q" + idx
        val valDef = VAL(valName) := REF(extractorName) APPLY LIT(paramName)
        QueryInterp(valDef, REF(valName) UNAPPLY REF(paramName), Some(extractor))
      case None =>
        param.ref match {
          case _: OptionDefinition =>
            QueryInterp(EmptyTree, interpolated("q_?", paramBaseInterp(paramName, param.baseDef)), None)
          case _ =>
            QueryInterp(EmptyTree, interpolated("q", paramBaseInterp(paramName, param)), None)
        }
    }

  }

  private def composeQueryParams(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Seq[QueryInterp] = {
    getFullParametersList(path, operation).collect {
      case param: QueryParameter => param
    }.zipWithIndex.map { case (param, idx) =>
      queryParamInterpSym(param, operation.operationId, idx)
    }
  }

  def generateMethodCall(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Tree = {

    val parameters = getMethodParameters(path, operation, withHeaders = false, withFormData = false).map {
      case (name, methodParam) if methodParam.defaultValue.nonEmpty && methodParam.isOptional =>
        REF(name) DOT "getOrElse" APPLY methodParam.defaultValue.get
      case (name, _) =>
        ID(name)
    }

    REF("controller") DOT operation.operationId APPLY (parameters: _ *)

  }

}
