package play.boilerplate.generators

import eu.unicredit.swagger.generators.{DefaultAsyncServerGenerator, SyntaxString}
import injection.InjectionProvider
import io.swagger.models.parameters.{BodyParameter, Parameter}
import io.swagger.models.{Operation, Swagger}
import security.SecurityProvider
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._

class PlayServerGenerator(routesGenerator: RoutesGenerator,
                          securityProvider: SecurityProvider,
                          injectionProvider: InjectionProvider)
  extends DefaultAsyncServerGenerator
    with SharedGeneratorCode {

  final val MIME_TYPE_JSON = "application/json"

  final val ACTION_ANYCONTENT: Type = TYPE_REF("Action") TYPE_OF "AnyContent"
  final val ACTION_EMPTY     : Type = TYPE_REF("Action") TYPE_OF UnitClass

  final val PARSER_ANYCONTENT: Tree = REF("parse") DOT "anyContent"
  final val PARSER_EMPTY     : Tree = REF("parse") DOT "empty"

  final val REQUEST_AS_JSON: Tree = REF("request") DOT "body" DOT "asJson"
  final val REQUEST_EMPTY  : Tree = SOME(UNIT)

  final def JSON_TO_TYPE(tpe: Type): Tree = WILDCARD DOT "as" APPLYTYPE tpe
  final def TYPE_TO_JSON(tpe: Type): Tree = REF("Json") DOT "toJson" APPLYTYPE tpe

  case class MimeTypeSupport(requestBody: Tree,
                             deserialize: Type => Tree,
                             serialize: Type => Tree)

  def getMimeTypeSupport: PartialFunction[String, MimeTypeSupport] = {
    case MIME_TYPE_JSON => MimeTypeSupport(REQUEST_AS_JSON, JSON_TO_TYPE, TYPE_TO_JSON)
  }

  override def generateRoutes(fileName: String, packageName: String): Option[String] = {
    val controllerFullName = packageName + ".controller." + controllerNameFromFileName(fileName)
    routesGenerator.generateRoutes(fileName, controllerFullName)
  }

  def composeMethods(swagger: Swagger, p: String): Seq[Tree] = {

    Option(swagger.getPath(p))
      .map { path =>

        val operations = getAllOperations(path)

        for {
          (method, op) <- operations.toSeq
          consumes = operationConsumes(swagger, op)
          produces = operationProduces(swagger, op)
        } yield generateMethod(method, op, consumes, produces)

      }
      .getOrElse(Nil)

  }

  def generateMethod(method: String,
                     operation: Operation,
                     consumes: Iterable[String],
                     produces: Iterable[String]): Tree = {

    val methodName = operation.getOperationId

    val bodyValues = generateValuesFromBody(operation.getParameters, produces.flatMap(mimeType => getMimeTypeSupport.lift(mimeType)))
    val methodParams = getMethodParamas(operation.getParameters)

    val actionSecurity = SecurityProvider.parseAction(operation, securityProvider)

    val (actionType, parser) = if (consumes.isEmpty || bodyValues.isEmpty) {
      (ACTION_EMPTY, PARSER_EMPTY)
    } else {
      (ACTION_ANYCONTENT, PARSER_ANYCONTENT)
    }

    val methodCall = REF("service") DOT methodName APPLY {
      (bodyValues.keys ++ methodParams.keys ++ actionSecurity.securityValues.keys).map(REF(_))
    }

    val ANSWER = methodCall INFIX "collect" APPLY generateAnswer(operation, consumes)

    val ERROR =
      REF("service") DOT "onError" APPLY (LIT(methodName), REF("cause")) INFIX "map" APPLY BLOCK {
        LAMBDA(PARAM("errAnswer").tree) ==> REF("InternalServerError") APPLY REF("errAnswer")
      }

    val ANSWER_WITH_EXCEPTION_HANDLE = ANSWER INFIX "recoverWith" APPLY BLOCK {
      CASE(REF("cause") withType RootClass.newClass("Throwable")) ==> ERROR
    }

    val methodValues =
      actionSecurity.securityValues.values.toIndexedSeq ++ bodyValues.values.toIndexedSeq

    val BODY_WITH_EXCEPTION_HANDLE =
      BLOCK {
        methodValues :+ ANSWER_WITH_EXCEPTION_HANDLE
      }

    val methodTree =
      DEFINFER(methodName) withParams methodParams.values withType actionType := BLOCK {
        actionSecurity.actionMethod(parser) APPLY {
          LAMBDA(PARAM("request").tree) ==> BODY_WITH_EXCEPTION_HANDLE
        }
      }

    methodTree.withDoc(
      Option(operation.getDescription).getOrElse("")
    )

  }

  final def generateValuesFromBody(params: Seq[Parameter], produces: Iterable[MimeTypeSupport]): Map[String, ValDef] = {

    if (produces.isEmpty) {
      Map.empty
    } else {
      params.collect {
        case bp: BodyParameter =>
          val tpe = noOptParamType(bp)
          val resolvers = produces.map { support =>
            support.requestBody INFIX "map" APPLY BLOCK {
              support.deserialize(tpe)
            }
          }
          val valDef = VAL(bp.getName) :=
            INFIX_CHAIN("orElse", resolvers) INFIX "getOrElse" APPLY BLOCK {
              THROW(IllegalArgumentExceptionClass, "Invalid body format")
            }
          bp.getName -> valDef
      }.toMap
    }

  }

  final def generateAnswer(operation: Operation, consumes: Iterable[String]): Tree = {

    val operationId = operation.getOperationId
    val responses = getOperationResponses(operation)
    val withoutDefault = !responses.exists(_.isDefault)
    val responseConsumes = consumes.filterNot(_ == MIME_TYPE_JSON)

    val cases = for (response <- responses) yield {
      val className = response.className(operationId)
      val status: Tree = response.status.map {
        name => REF(name)
      }.getOrElse {
        REF("Status") APPLY REF("status")
      }
      (response.body, Some(IntClass).filter(_ => response.isDefault)) match {
        case (Some(body), Some(_)) =>
          val default = status APPLY (TYPE_TO_JSON(body) APPLY REF("answer"))
          CASE(REF(className) UNAPPLY (ID("answer"), ID("status"))) ==>
            generateResponse(status, body, responseConsumes, default)
        case (Some(body), None) =>
          val default = status APPLY (TYPE_TO_JSON(body) APPLY REF("answer"))
          CASE(REF(className) UNAPPLY ID("answer")) ==>
            generateResponse(status, body, responseConsumes, default)
        case (None, Some(_)) =>
          val default = status
          CASE(REF(className) UNAPPLY ID("status")) ==>
            generateResponse(status, UnitClass, Nil, default)
        case (None, None) =>
          val default = status
          CASE(REF(className)) ==>
            generateResponse(status, UnitClass, Nil, default)
      }
    }

    val UnexpectedResultCase = if (withoutDefault) {
      Some(
        CASE(REF(UnexpectedResult) UNAPPLY(ID("answer"), ID("status"))) ==>
          REF("Status") APPLY REF("status") APPLY REF("answer")
      )
    } else {
      None
    }

    BLOCK {
      cases ++ UnexpectedResultCase
    }

  }

  final def generateResponse(status: Tree, tpe: Type, consumes: Iterable[String], default: Tree): Tree = {

    if (consumes.isEmpty) {
      default
    } else {
      val matchers = consumes.zipWithIndex.map { case (mimeType, idx) =>
        val matcherDef = VAL(s"m$idx") := (REF("AcceptMatcher") APPLY LIT(mimeType))
        mimeType -> matcherDef
      }.toMap
      val cases = matchers.map { case (mimeType, matcher) =>
        CASE(REF(matcher.name) UNAPPLY WILDCARD) ==>
          (status APPLY (getMimeTypeSupport(mimeType).serialize(tpe) APPLY REF("answer")))
      }.toSeq :+ {
        CASE(WILDCARD) ==> default
      }
      BLOCK(
        matchers.values.toSeq :+ (REF("request") MATCH cases)
      )
    }

  }

  final def generateAcceptMatcher: Tree = {
    val tpe = (OptionClass APPLYTYPE StringClass.toType).tpe
    BLOCK(
      IMPORT("play.api.mvc", "_"),
      CASECLASSDEF("AcceptMatcher") withParams PARAM("mimeType", StringClass.toType).tree := BLOCK {
        DEF("unapply", tpe) withParams PARAM("arg", TYPE_REF("RequestHeader")).tree := BLOCK {
          IF((REF("arg") DOT "acceptedTypes" DOT "nonEmpty") AND (REF("arg") DOT "accepts" APPLY REF("mimeType"))) THEN BLOCK {
            SOME(REF("mimeType"))
          } ELSE BLOCK {
            NONE
          }
        }
      }
    )
  }

  def generateHelpers(packageName: String): Seq[Tree] = Seq(generateAcceptMatcher)

  final def generatePackageObject(packageName: String, objectName: String): SyntaxString = {
    val imports = EmptyTree inPackage packageName
    val objectTree = OBJECTDEF(objectName).withFlags(Flags.PACKAGE) := BLOCK {
      generateHelpers(objectName)
    }
    SyntaxString(objectName, treeToString(imports), treeToString(objectTree))
  }

  final override def generateImports(packageName: String, codeProvidedPackage: String, controllerName: String): Seq[Import] = {
    Seq(
      IMPORT(packageName, "_"),
      IMPORT(packageName + ".json", "_"),
      IMPORT("play.api.mvc", "_"),
      IMPORT("play.api.libs.json", "_"),
      IMPORT("play.api.libs.concurrent.Execution.Implicits", "_")
    ) ++
      Seq(codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_"))
  }

  def imports(fileName: String, packageName: String, codeProvidedPackage: String): Seq[Import] = {

    val serviceGenerator = new PlayServiceGenerator(securityProvider)
    val servicePackageName = serviceGenerator.servicePackageName(packageName)
    val serviceName = serviceGenerator.serviceNameFromFileName(fileName)

    val addImports = (securityProvider.controllerImports ++ injectionProvider.imports) ++ Seq(
      IMPORT("_root_." + servicePackageName, serviceName),
      IMPORT(serviceName, "_")
    )

    generateImports(packageName, codeProvidedPackage, controllerNameFromFileName(fileName)) ++ addImports

  }

  def dependencies(fileName: String, packageName: String, codeProvidedPackage: String): Seq[InjectionProvider.Dependency] = {

    val serviceGenerator = new PlayServiceGenerator(securityProvider)
    val serviceName = serviceGenerator.serviceNameFromFileName(fileName)

    Seq(InjectionProvider.Dependency("service", TYPE_REF(serviceName)))

  }

  override def generate(fileName: String, packageName: String, codeProvidedPackage: String): Iterable[SyntaxString] = {

    parseSwagger(fileName).map { swagger =>

      val completePaths = swagger.getPaths.keySet().toSeq

      val methods = completePaths.flatMap(p => composeMethods(swagger, p))

      if (methods.nonEmpty) {

        val controllerPackageName = packageName + ".controller"
        val controllerName = controllerNameFromFileName(fileName)

        val controllerImports = BLOCK {
          imports(fileName, packageName, codeProvidedPackage)
        } inPackage controllerPackageName

        val classDef = CLASSDEF(controllerName)
          .withParents(TYPE_REF("Controller") +: securityProvider.controllerParents)
          .withSelf("self", securityProvider.controllerSelfTypes: _ *) :=
          BLOCK {
            methods
          }

        // --- DI
        val controllerTree = injectionProvider.classDefModifier(classDef, dependencies(fileName, packageName, codeProvidedPackage))

        SyntaxString(controllerName, treeToString(controllerImports), controllerTree) :: Nil

      } else {
        Nil
      }

    }.getOrElse(Nil) :+
      generatePackageObject(packageName, "controller")

  }

}