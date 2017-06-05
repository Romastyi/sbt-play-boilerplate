package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._

class PlayClientGeneratorParser {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(ctx.basePackageName, "_"),
      IMPORT(ctx.basePackageName + ".json", "_"),
      IMPORT(ctx.servicePackageName, ctx.serviceClassName),
      IMPORT(ctx.serviceClassName, "_"),
      IMPORT("play.api.http.HeaderNames", "_"),
      IMPORT("play.api.libs.ws", "_"),
      IMPORT("play.api.libs.json", "_"),
      IMPORT("play.api.libs.concurrent.Execution.Implicits", "_"),
      IMPORT("scala.concurrent", "Future")
    ) ++
      ctx.securityProvider.controllerImports ++
      ctx.injectionProvider.imports ++
      Seq(ctx.codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_"))
  }

  def dependencies(implicit cxt: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(InjectionProvider.Dependency("WS", TYPE_REF("WSClient")))
  }

  def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[SyntaxString] = {

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(schema, path, operation)(ctx.addCurrentPath(operation.operationId).setInClient(true))

    val clientSources = if (methods.nonEmpty) {

      val clientImports = BLOCK {
        generateImports
      } inPackage ctx.clientPackageName

      val classDef = CLASSDEF(ctx.clientClassName)
        .withParents(TYPE_REF(ctx.serviceClassName))
        .withParams(
          PARAM("baseUrl", StringClass).empty,
          PARAM("headers", repeatedParamType(TYPE_TUPLE(StringClass, StringClass))).empty
        )
        .withSelf("self") :=
        BLOCK {
          methods.toIndexedSeq :+ generateOrErrorMethod
        }

      // --- DI
      val clientTree = ctx.injectionProvider.classDefModifier(classDef, dependencies)

      SyntaxString(ctx.clientClassName, treeToString(clientImports), clientTree) :: Nil

    } else {
      Nil
    }

    clientSources ++ generatePackageObject

  }

  def generateMethod(schema: Schema, path: Path, operation: Operation)(implicit ctx: GeneratorContext): Tree = {

    val bodyParams = getBodyParameters(path, operation)
    val methodParams = getMethodParameters(path, operation)
    val securityParams = ctx.securityProvider.getActionSecurity(operation.security.toIndexedSeq).securityParams
    val fullBodyParams = bodyParams.keys.map {
      name => name -> (REF("Json") DOT "toJson" APPLY REF(name))
    }.toMap

    //probably to be fixed with a custom ordering
    val urlParams: Seq[Tree] = (path.parameters ++ operation.parameters).toSeq collect {
      case query: QueryParameter =>
        val name = query.name
        val ref = query.ref match {
          case _: OptionDefinition => SOME(REF(name))
          case _ => REF(name)
        }
        LIT(name) INFIX ("->", ref)
    }

    val headerParams: Seq[Tree] = (path.parameters ++ operation.parameters).toSeq collect {
      case param: HeaderParameter =>
        val name = param.name
        val ref = param.ref match {
          case _: OptionDefinition => SOME(REF(name))
          case _ => REF(name)
        }
        LIT(name) INFIX ("->", ref)
    }

    val baseUrl =
      INTERP("s", LIT(cleanDuplicateSlash("$baseUrl/" + doClientUrl(schema.basePath, path.pathParts))))
    val baseUrlWithParams = if (urlParams.isEmpty) {
      baseUrl
    } else {
      baseUrl INFIX("+", REF("_render_url_params") APPLY (urlParams: _*))
    }

    val wsUrl = REF("WS") DOT "url" APPLY baseUrlWithParams
    val wsUrlWithAccept = wsUrl DOT "withHeaders" APPLY (REF("ACCEPT") INFIX ("->", LIT("application/json")))
    val wsUrlWithHeaderParams = if (headerParams.isEmpty) {
      wsUrlWithAccept
    } else {
      wsUrlWithAccept DOT "withHeaders" APPLY SEQARG(REF("_render_header_params") APPLY (headerParams: _*))
    }
    val wsUrlWithHeaders = wsUrlWithHeaderParams DOT "withHeaders" APPLY SEQARG(REF("headers"))

    val RuntimeExceptionClass = definitions.getClass("java.lang.RuntimeException")

    val ERROR =
      REF("self") DOT "onError" APPLY (LIT(operation.operationId), REF("cause")) INFIX "map" APPLY BLOCK {
        LAMBDA(PARAM("errAnswer").tree) ==> THROW(NEW(RuntimeExceptionClass, REF("errAnswer"), REF("cause")))
      }

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val opType = operation.httpMethod.toString.toLowerCase
    val methodTree = DEF(operation.operationId, FUTURE(methodType))
      .withFlags(Flags.OVERRIDE)
      .withParams(bodyParams.values.map(_.valDef) ++ methodParams.values.map(_.valDef) ++ securityParams.values) :=
      BLOCK {
        wsUrlWithHeaders DOT opType APPLY fullBodyParams.values DOT "map" APPLY {
          LAMBDA(PARAM("resp").tree) ==> BLOCK(generateResponses(operation))
        } DOT "recoverWith" APPLY BLOCK {
          CASE(REF("cause") withType RootClass.newClass("Throwable")) ==> ERROR
        }
      }

    methodTree.withDoc(
      s"""${Option(operation.description).getOrElse("")}
         |
         """.stripMargin
    )

  }

  def generateResponses(operation: Operation)(implicit ctx: GeneratorContext): Tree = {

    val responses = operation.responses.toSeq.sortBy {
      case (DefaultResponse, _) => 2
      case _ => 1
    }

    val cases = for ((code, response) <- responses) yield {
      val className = getResponseClassName(operation.operationId, code)
      val bodyParam = response.schema.map { body =>
        REF("resp") DOT "json" DOT "as" APPLYTYPE getTypeSupport(body).tpe
      }
      code match {
        case DefaultResponse =>
          CASE(ID("status")) ==> (REF(className) APPLY (bodyParam.toSeq :+ REF("status")))
        case StatusResponse(status) =>
          CASE(LIT(status)) ==> bodyParam.map(bp => REF(className) APPLY bp).getOrElse(REF(className))
      }
    }

    val UnexpectedResultCase = if (operation.responses.keySet(DefaultResponse)) {
      None
    } else {
      Some(
        CASE(ID("status")) ==> (REF(UnexpectedResult) APPLY (REF("resp") DOT "body", REF("status")))
      )
    }

    REF("resp") DOT "status" MATCH {
      cases ++ UnexpectedResultCase
    }

  }

  def generateOrErrorMethod: Tree = {

    val operationId: ValDef = PARAM("operationId", StringClass.toType).tree
    val cause      : ValDef = PARAM("cause", RootClass.newClass("Throwable")).tree

    val methodTree = DEF("onError", FUTURE(StringClass.toType))
      .withFlags(Flags.OVERRIDE)
      .withParams(operationId, cause) :=
      BLOCK {
        REF("Future") DOT "successful" APPLY {
          INFIX_CHAIN("+",
            LIT("Operation '"),
            REF("operationId"),
            LIT("' error: "),
            REF("cause") DOT "getMessage"
          )
        }
      }

    methodTree.withDoc(
      "Error handler",
      DocTag.Param("operationId", "Operation where error was occurred"),
      DocTag.Param("cause"      , "An occurred error")
    )

  }

  final def generateRenderUrlParams(packageName: String): Tree = {
    DEFINFER("_render_url_params")
      .withFlags(PRIVATEWITHIN(packageName))
      .withParams(PARAM("pairs", TYPE_*(TYPE_TUPLE(StringClass, OptionClass TYPE_OF AnyClass))).tree) :=
      BLOCK(
        Seq(
          VAL("parts") := (
            REF("pairs")
              DOT "collect" APPLY BLOCK(
              CASE(TUPLE(ID("k"), REF("Some") UNAPPLY ID("v"))) ==> (REF("k") INFIX("+", LIT("=")) INFIX("+", REF(
                "v"))))
            ),
          IF(REF("parts") DOT "nonEmpty")
            THEN (
            REF("parts") DOT "mkString" APPLY(LIT("?"), LIT("&"), LIT(""))
            )
            ELSE LIT("")
        )
      )
  }

  final def generateRenderHeaderParams(packageName: String): Tree = {
    DEFINFER("_render_header_params")
      .withFlags(PRIVATEWITHIN(packageName))
      .withParams(PARAM("pairs", TYPE_*(TYPE_TUPLE(StringClass, OptionClass TYPE_OF AnyClass))).tree) :=
      BLOCK(
        Seq(
          REF("pairs")
            DOT "collect" APPLY BLOCK(CASE(TUPLE(ID("k"), REF("Some") UNAPPLY ID("v"))) ==>
            (REF("k") INFIX ("->", REF("v") DOT "toString")))
        )
      )
  }

  final def generateUnexpectedResponseStatus: Tree = {
    CASECLASSDEF("UnexpectedResponseStatus")
      .withParams(PARAM("status", IntClass).tree, PARAM("body", StringClass).tree)
      .withParents(ThrowableClass, definitions.getClass("scala.util.control.NoStackTrace")) :=
      BLOCK {
        DEF("getMessage", StringClass).withFlags(Flags.OVERRIDE) := BLOCK {
          INFIX_CHAIN("+",
            LIT("unexpected response status: "),
            REF("status"),
            LIT(" "),
            REF("body")
          )
        }
      }
  }

  def generateHelpers(packageName: String)(implicit ctx: GeneratorContext): Seq[Tree] = Seq(
    generateUnexpectedResponseStatus,
    generateRenderUrlParams(packageName),
    generateRenderHeaderParams(packageName)
  )

  final def generatePackageObject(implicit ctx: GeneratorContext): Seq[SyntaxString] = {

    val objectName = ctx.clientPackageName.split('.').last
    val helpers = generateHelpers(objectName)

    if (helpers.nonEmpty) {
      val imports = EmptyTree inPackage ctx.clientPackageName
      val objectTree = OBJECTDEF(objectName).withFlags(Flags.PACKAGE) := BLOCK(helpers)
      SyntaxString(objectName, treeToString(imports), treeToString(objectTree)) :: Nil
    } else {
      Nil
    }

  }

}
