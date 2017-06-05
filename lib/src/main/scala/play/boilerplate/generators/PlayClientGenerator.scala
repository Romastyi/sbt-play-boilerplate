package play.boilerplate.generators

import eu.unicredit.swagger.StringUtils
import eu.unicredit.swagger.generators.{DefaultClientGenerator, SyntaxString}
import injection.InjectionProvider
import io.swagger.models.{Model, Operation, Swagger}
import security.SecurityProvider
import treehugger.forest._
import definitions._
import io.swagger.models.parameters.{HeaderParameter, QueryParameter}
import play.boilerplate.ParserUtils
import treehuggerDSL._

import scala.collection.JavaConversions._

class PlayClientGenerator(codeProvidedPackage: String,
                          securityProvider: SecurityProvider = SecurityProvider.default,
                          injectionProvider: InjectionProvider = new InjectionProvider.DefaultInConstructor())
  extends DefaultClientGenerator
    with SharedGeneratorCode {

  def composeMethods(swagger: Swagger, p: String): Seq[Tree] = {

    Option(swagger.getPath(p))
      .map { path =>

        val basePath = Option(swagger.getBasePath).getOrElse("/")
        val models = getDefinitions(swagger)
        val operations = getAllOperations(path)

        for {
          (method, op) <- operations.toSeq
          url = StringUtils.doUrl(basePath, p)
          consumes = operationConsumes(swagger, op)
          produces = operationProduces(swagger, op)
        } yield generateMethod(method.toLowerCase(), url, op, consumes, produces, models)

      }
      .getOrElse(Nil)

  }

  def generateMethod(opType: String,
                     url: String,
                     operation: Operation,
                     consumes: Iterable[String],
                     produces: Iterable[String],
                     models: Map[String, Model]): Tree = {

    val methodName = operation.getOperationId

    val parameters = Option(operation.getParameters).map(_.toList).getOrElse(Nil)
    val bodyParams = generateParamsFromBody(methodName, parameters, models)
    val methodParams = generateMethodParams(methodName, parameters, models)
    val securityParams = SecurityProvider.parseAction(operation, securityProvider).securityParams
    val fullBodyParams = getParamsToBody(parameters)

    //probably to be fixed with a custom ordering
    val urlParams: Seq[Tree] =
      parameters collect {
        case query: QueryParameter =>
          val name = query.getName
          LIT(name) INFIX ("->", if (query.getRequired) SOME(REF(name)) else REF(name))
      }

    val headerParams: Seq[Tree] =
      parameters collect {
        case param: HeaderParameter =>
          val name = param.getName
          LIT(name) INFIX ("->", if (param.getRequired) SOME(REF(name)) else REF(name))
      }

    val baseUrl =
      INTERP("s", LIT(StringUtils.cleanDuplicateSlash("$baseUrl/" + StringUtils.cleanPathParams(url))))
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

/*
    val response = getOkRespType(operation) getOrElse {
      throw new Exception(s"Cannot determine Ok result type for $methodName")
    }
*/

    val RuntimeExceptionClass = definitions.getClass("java.lang.RuntimeException")

    val ERROR =
      REF("self") DOT "onError" APPLY (LIT(methodName), REF("cause")) INFIX "map" APPLY BLOCK {
        LAMBDA(PARAM("errAnswer").tree) ==> THROW(NEW(RuntimeExceptionClass, REF("errAnswer"), REF("cause")))
      }

    val methodType = TYPE_REF(getOperationResponseTraitName(methodName))

    val methodTree = DEF(methodName, FUTURE(methodType))
      .withFlags(Flags.OVERRIDE)
      .withParams(bodyParams.values.map(_.valDef) ++ methodParams.values.map(_.valDef) ++ securityParams.values) :=
      BLOCK {
        wsUrlWithHeaders DOT opType APPLY fullBodyParams.values DOT "map" APPLY {
          LAMBDA(PARAM("resp").tree) ==> BLOCK(generateResponses(operation, models))
        } DOT "recoverWith" APPLY BLOCK {
          CASE(REF("cause") withType RootClass.newClass("Throwable")) ==> ERROR
        }
      }

    methodTree.withDoc(
      s"""${Option(operation.getDescription).getOrElse("")}
         |
         """.stripMargin
    )

  }

  def generateResponses(operation: Operation, models: Map[String, Model]): Tree = {

    val operationId = operation.getOperationId
    val responses = getOperationResponses(operation, models)
    val withoutDefault = !responses.exists(_.isDefault)

    val cases = for (response <- responses.sortBy(_.isDefault)) yield {
      val className = response.className(operationId)
      val bodyParam = response.body.map { tpe =>
        REF("resp") DOT "json" DOT "as" APPLYTYPE tpe
      }
      if (response.isDefault) {
        CASE(ID("status")) ==> (REF(className) APPLY (bodyParam.toSeq :+ REF("status")))
      } else {
        CASE(LIT(response.code)) ==> bodyParam.map(bp => REF(className) APPLY bp).getOrElse(REF(className))
      }
    }

    val UnexpectedResultCase = if (withoutDefault) {
      Some(
        CASE(ID("status")) ==> (REF(UnexpectedResult) APPLY (REF("resp") DOT "body", REF("status")))
      )
    } else {
      None
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

  def generateHelpers(packageName: String): Seq[Tree] = Seq(
    generateUnexpectedResponseStatus,
    generateRenderUrlParams(packageName),
    generateRenderHeaderParams(packageName)
  )

  final def generatePackageObject(packageName: String, objectName: String): SyntaxString = {
    val imports = EmptyTree inPackage packageName
    val objectTree = OBJECTDEF(objectName).withFlags(Flags.PACKAGE) := BLOCK {
      generateHelpers(objectName)
    }
    SyntaxString(objectName, treeToString(imports), treeToString(objectTree))
  }

  final def generateImports(packageName: String): Seq[Import] = {
    Seq(
      IMPORT(packageName, "_"),
      IMPORT(packageName + ".json", "_"),
      IMPORT("play.api.http.HeaderNames", "_"),
      IMPORT("play.api.libs.ws", "_"),
      IMPORT("play.api.libs.json", "_"),
      IMPORT("play.api.libs.concurrent.Execution.Implicits", "_"),
      IMPORT("scala.concurrent", "Future")
    ) ++
      Seq(codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_"))
  }

  def imports(fileName: String, packageName: String): Seq[Import] = {

    val serviceGenerator = new PlayServiceGenerator(securityProvider)
    val servicePackageName = serviceGenerator.servicePackageName(packageName)
    val serviceName = serviceGenerator.serviceNameFromFileName(fileName)

    val addImports = (securityProvider.controllerImports ++ injectionProvider.imports) ++ Seq(
      IMPORT("_root_." + servicePackageName, serviceName),
      IMPORT(serviceName, "_")
    )

    generateImports(packageName) ++ addImports

  }

  def dependencies(fileName: String, packageName: String): Seq[InjectionProvider.Dependency] = {
    Seq(InjectionProvider.Dependency("WS", TYPE_REF("WSClient")))
  }

  override def generate(fileName: String, packageName: String): Iterable[SyntaxString] = {

    ParserUtils.parseSwagger(fileName).map { swagger =>

      val completePaths = swagger.getPaths.keySet().toSeq

      val methods = completePaths.flatMap(p => composeMethods(swagger, p))

      if (methods.nonEmpty) {

        val clientPackageName = packageName + ".client"
        val clientName = clientNameFromFileName(fileName)

        val serviceGenerator = new PlayServiceGenerator(securityProvider)
        val serviceName = serviceGenerator.serviceNameFromFileName(fileName)

        val clientImports = BLOCK {
          imports(fileName, packageName)
        } inPackage clientPackageName

        val classDef = CLASSDEF(clientName)
          .withParents(TYPE_REF(serviceName))
          .withParams(
            PARAM("baseUrl", StringClass).empty,
            PARAM("headers", repeatedParamType(TYPE_TUPLE(StringClass, StringClass))).empty
          )
          .withSelf("self") :=
          BLOCK {
            methods :+ generateOrErrorMethod
          }

        // --- DI
        val clientTree = injectionProvider.classDefModifier(classDef, dependencies(fileName, packageName))

        SyntaxString(clientName, treeToString(clientImports), clientTree) :: Nil

      } else {
        Nil
      }

    }.getOrElse(Nil) :+
      generatePackageObject(packageName, "client")

  }

}