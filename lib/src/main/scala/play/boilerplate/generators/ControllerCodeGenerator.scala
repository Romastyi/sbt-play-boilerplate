package play.boilerplate.generators

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._
import treehugger.forest._
import definitions._
import treehuggerDSL._

class ControllerCodeGenerator extends CodeGenerator {

  import GeneratorUtils._

  def generateImports(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF(ctx.settings.jsonImportPrefix), "_"),
      IMPORT(REF(ctx.settings.servicePackageName), ctx.settings.serviceClassName),
      IMPORT(REF(ctx.settings.serviceClassName), "_"),
      IMPORT(REF("play.api.mvc"), "_"),
      IMPORT(REF("play.api.libs.json"), "_"),
      IMPORT(REF("play.api.libs.concurrent.Execution.Implicits"), "_")
    ) ++
      ctx.settings.securityProvider.controllerImports ++
      ctx.settings.injectionProvider.imports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
  }

  def dependencies(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(InjectionProvider.Dependency("service", TYPE_REF(ctx.settings.serviceClassName))) ++
      ctx.settings.securityProvider.controllerDependencies
  }

  def baseControllerType: Type = TYPE_REF("Controller")

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(path, operation)(ctx.addCurrentPath(operation.operationId).setInClient(true))

    val controllerSources = if (methods.nonEmpty) {

      val controllerImports = BLOCK {
        generateImports
      } inPackage ctx.settings.controllerPackageName

      val companionItems = filterNonEmptyTree(methods.flatMap(_.implicits))

      val (companionObj, importCompanion) = if (companionItems.nonEmpty) {
        val objDef = OBJECTDEF(ctx.settings.controllerClassName) := BLOCK {
          companionItems
        }
        (objDef, IMPORT(REF(ctx.settings.controllerClassName), "_"))
      } else {
        (EmptyTree, EmptyTree)
      }

      val parents = Seq(baseControllerType, TYPE_REF("ControllerHelper")) ++
        ctx.settings.securityProvider.controllerParents

      val classDef = CLASSDEF(ctx.settings.controllerClassName)
        .withParents(parents)
        .withSelf("self", ctx.settings.securityProvider.controllerSelfTypes: _ *) :=
        BLOCK {
          filterNonEmptyTree(importCompanion +: methods.map(_.tree).toIndexedSeq)
        }

      // --- DI
      val controllerTree = ctx.settings.injectionProvider.classDefModifier(classDef, dependencies)

      SourceCodeFile(
        packageName = ctx.settings.controllerPackageName,
        className = ctx.settings.controllerClassName,
        header = treeToString(controllerImports),
        impl = controllerTree + "\n\n" + treeToString(companionObj)
      ) :: Nil

    } else {
      Nil
    }

    controllerSources ++ generateHelperTrait

  }

  case class Method(tree: Tree, implicits: Seq[Tree])

  def generateMethod(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val methodName = operation.operationId

    val bodyValues = generateValuesFromBody(operation.requestBody.map(_.content).getOrElse(Map.empty))
    val methodParams = getMethodParameters(path, operation)

    val actionSecurity = ctx.settings.securityProvider.getActionSecurity(operation.security.toIndexedSeq)

    val (actionType, parser) = if (bodyValues.isEmpty) {
      (ACTION_EMPTY, PARSER_EMPTY)
    } else {
      (ACTION_ANYCONTENT, PARSER_ANYCONTENT)
    }

    val methodCall = REF("service") DOT methodName APPLY {
      (bodyValues.keys ++ methodParams.keys ++ actionSecurity.securityValues.keys).map(REF(_))
    }

    val answerMethod = generateAnswer(operation)
    val ANSWER = methodCall INFIX "collect" APPLY answerMethod.tree

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
      DEFINFER(methodName) withParams methodParams.values.map(_.valDef) withType actionType := BLOCK {
        actionSecurity.actionMethod(parser) APPLY {
          LAMBDA(PARAM("request").tree) ==> BODY_WITH_EXCEPTION_HANDLE
        }
      }

    val tree = methodTree.withDoc(
      operation.description.getOrElse("")
    )

    val implicits = methodParams.values.flatMap(_.implicits).toSeq

    Method(tree, answerMethod.implicits ++ implicits)

  }

  final def generateValuesFromBody(requestContent: Map[String, Definition])
                                  (implicit ctx: GeneratorContext): Map[String, ValDef] = {

    val supportedConsumes = requestContent.keys.flatMap(mimeType => getMimeTypeSupport.lift(mimeType))

    if (supportedConsumes.isEmpty) {
      Map.empty
    } else {
      // TODO AST for request bodies

/*
      parameters.collect {
        case bp: BodyParameter =>
          val tpe = getTypeSupport(bp.ref).tpe
          val resolvers = produces.map { support =>
            support.requestBody INFIX "map" APPLY BLOCK(
              LAMBDA(PARAM("body").tree) ==> support.deserialize(tpe)(REF("body"))
            )
          }
          val valName = decapitalize(bp.name)
          val valDef = VAL(valName) :=
            INFIX_CHAIN("orElse", resolvers) INFIX "getOrElse" APPLY BLOCK {
              THROW(IllegalArgumentExceptionClass, "Invalid body format")
            }
          valName -> valDef
      }.toMap
*/
    }

  }

  final def generateAnswer(operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val supportedProduces = operation.produces.filterNot(_ == MIME_TYPE_JSON).flatMap(
      mimeType => getMimeTypeSupport.lift(mimeType)
    )

    val cases = for ((code, response) <- operation.responses) yield {
      val className = getResponseClassName(operation.operationId, code)
      val statusCode = Some(code).flatMap {
        case DefaultResponse => None
        case StatusResponse(c) => Some(c)
      }
      val status: Tree = statusCode.flatMap(getStatusByCode).map {
        name => REF(name)
      }.getOrElse {
        REF("Status") APPLY REF("status")
      }
      val bodySupport = response.content.map(body => getTypeSupport(body))
      val tree = (bodySupport.map(_.tpe), Some(IntClass).filter(_ => code == DefaultResponse)) match {
        case (Some(body), Some(_)) =>
          val default = status APPLY TYPE_TO_JSON(body)(REF("answer"))
          CASE(REF(className) UNAPPLY (ID("answer"), ID("status"))) ==>
            generateResponse(status, body, supportedProduces, default)
        case (Some(body), None) =>
          val default = status APPLY TYPE_TO_JSON(body)(REF("answer"))
          CASE(REF(className) UNAPPLY ID("answer")) ==>
            generateResponse(status, body, supportedProduces, default)
        case (None, Some(_)) =>
          val default = status
          CASE(REF(className) UNAPPLY ID("status")) ==>
            generateResponse(status, UnitClass, Nil, default)
        case (None, None) =>
          val default = status
          CASE(REF(className)) ==>
            generateResponse(status, UnitClass, Nil, default)
      }
      Method(tree, bodySupport.map(s => s.jsonReads ++ s.jsonWrites).getOrElse(Nil))
    }

    val UnexpectedResultCase = if (operation.responses.keySet(DefaultResponse)) {
      None
    } else {
      Some(
        CASE(REF(UnexpectedResult) UNAPPLY(ID("answer"), ID("status"))) ==>
          REF("Status") APPLY REF("status") APPLY REF("answer")
      )
    }

    val tree = BLOCK {
      cases.map(_.tree) ++ UnexpectedResultCase
    }

    Method(tree, cases.flatMap(_.implicits))

  }

  private def generateResponse(status: Tree, tpe: Type, produces: Iterable[MimeTypeSupport], default: Tree): Tree = {

    if (produces.isEmpty) {
      default
    } else {
      val matchers = produces.zipWithIndex.map { case (support, idx) =>
        val matcherDef = VAL(s"m$idx") := (REF("AcceptMatcher") APPLY LIT(support.mimeType))
        support -> matcherDef
      }.toMap
      val cases = matchers.map { case (support, matcher) =>
        CASE(REF(matcher.name) UNAPPLY WILDCARD) ==> {
          status APPLY support.serialize(tpe)(REF("answer"))
        }
      }.toSeq :+ {
        CASE(WILDCARD) ==> default
      }
      BLOCK(
        matchers.values.toSeq :+ (REF("request") MATCH cases)
      )
    }

  }

  def generateHelpers(implicit ctx: GeneratorContext): Seq[Tree] = Seq(generateAcceptMatcher)

  final def generateHelperTrait(implicit ctx: GeneratorContext): Seq[CodeFile] = {

    val helpers = generateHelpers

    if (helpers.nonEmpty) {
      val imports = EmptyTree inPackage ctx.settings.controllerPackageName
      val objectTree = TRAITDEF("ControllerHelper") := BLOCK(helpers)
      SourceCodeFile(
        packageName = ctx.settings.controllerPackageName,
        className = "ControllerHelper",
        header = treeToString(imports),
        impl = treeToString(objectTree)
      ) :: Nil
    } else {
      Nil
    }

  }

}

object InjectedControllerCodeGenerator extends ControllerCodeGenerator {
  override def baseControllerType: Type = TYPE_REF("InjectedController")
}