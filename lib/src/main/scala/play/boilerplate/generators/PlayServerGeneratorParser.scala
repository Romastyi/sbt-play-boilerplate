package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._

class PlayServerGeneratorParser(schema: Schema) {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(ctx.modelPackageName, "_"),
      IMPORT(ctx.jsonPackageName , "_"),
      IMPORT(ctx.servicePackageName, ctx.serviceClassName),
      IMPORT(ctx.serviceClassName, "_"),
      IMPORT("play.api.mvc", "_"),
      IMPORT("play.api.libs.json", "_"),
      IMPORT("play.api.libs.concurrent.Execution.Implicits", "_")
    ) ++
      ctx.securityProvider.controllerImports ++
      ctx.injectionProvider.imports ++
      Seq(ctx.codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_"))
  }

  def dependencies(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(InjectionProvider.Dependency("service", TYPE_REF(ctx.serviceClassName)))
  }

  def generate(implicit ctx: GeneratorContext): Iterable[SyntaxString] = {

    val methods = for {
      path <- schema.paths
      op <- path.operations.values
    } yield generateMethod(path, op)(ctx.addCurrentPath(op.operationId).setInController(true))

    val controllerSources = if (methods.nonEmpty) {

      val controllerImports = BLOCK {
        generateImports
      } inPackage ctx.controllerPackageName

      val companionItems = methods.flatMap(_.implicits)

      val (companionObj, importCompanion) = if (companionItems.nonEmpty) {
        val objDef = OBJECTDEF(ctx.controllerClassName) := BLOCK {
          companionItems
        }
        (objDef, IMPORT(ctx.controllerClassName, "_"))
      } else {
        (EmptyTree, EmptyTree)
      }

      val classDef = CLASSDEF(ctx.controllerClassName)
        .withParents(TYPE_REF("Controller") +: ctx.securityProvider.controllerParents)
        .withSelf("self", ctx.securityProvider.controllerSelfTypes: _ *) :=
        BLOCK {
          filterNonEmptyTree(importCompanion +: methods.map(_.tree).toIndexedSeq)
        }

      // --- DI
      val controllerTree = ctx.injectionProvider.classDefModifier(classDef, dependencies)

      SyntaxString(ctx.controllerClassName, treeToString(controllerImports), controllerTree + "\n" + treeToString(companionObj)) :: Nil

    } else {
      Nil
    }

    controllerSources ++ generatePackageObject

  }

  case class Method(tree: Tree, implicits: Seq[Tree])

  def generateMethod(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val methodName = operation.operationId

    val supportedProduces = operation.produces.flatMap(mimeType => getMimeTypeSupport.lift(mimeType))
    val bodyValues = generateValuesFromBody(operation.parameters, supportedProduces)
    val methodParams = getMethodParameters(path, operation)

    val actionSecurity = ctx.securityProvider.getActionSecurity(operation.security.toIndexedSeq)

    val (actionType, parser) = if (operation.consumes.isEmpty || bodyValues.isEmpty) {
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

  final def generateValuesFromBody(parameters: Iterable[Parameter], produces: Iterable[MimeTypeSupport])
                                  (implicit ctx: GeneratorContext): Map[String, ValDef] = {

    if (produces.isEmpty) {
      Map.empty
    } else {
      parameters.collect {
        case bp: BodyParameter =>
          val tpe = getTypeSupport(bp.ref).tpe
          val resolvers = produces.map { support =>
            support.requestBody INFIX "map" APPLY BLOCK {
              support.deserialize(tpe)
            }
          }
          val valDef = VAL(bp.name) :=
            INFIX_CHAIN("orElse", resolvers) INFIX "getOrElse" APPLY BLOCK {
              THROW(IllegalArgumentExceptionClass, "Invalid body format")
            }
          bp.name -> valDef
      }.toMap
    }

  }

  final def generateAnswer(operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val responseConsumes = operation.consumes.filterNot(_ == MIME_TYPE_JSON)

    val cases = for ((code, response) <- operation.responses) yield {
      val className = getOperationResponseTraitName(operation.operationId)
      val statusCode = Some(code).flatMap {
        case DefaultResponse => None
        case StatusResponse(c) => Some(c)
      }
      val status: Tree = statusCode.flatMap(getStatusByCode).map {
        name => REF(name)
      }.getOrElse {
        REF("Status") APPLY REF("status")
      }
      val bodySupport = response.schema.map(body => getTypeSupport(body))
      val tree = (bodySupport.map(_.tpe), Some(IntClass).filter(_ => code == DefaultResponse)) match {
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

    Method(BLOCK(cases.map(_.tree) ++ UnexpectedResultCase), cases.flatMap(_.implicits))

  }

  private def generateResponse(status: Tree, tpe: Type, consumes: Iterable[String], default: Tree): Tree = {

    if (consumes.isEmpty) {
      default
    } else {
      val matchers = consumes.zipWithIndex.map { case (mimeType, idx) =>
        val matcherDef = VAL(s"m$idx") := (REF("AcceptMatcher") APPLY LIT(mimeType))
        mimeType -> matcherDef
      }.toMap
      val cases = matchers.map { case (mimeType, matcher) =>
        CASE(REF(matcher.name) UNAPPLY WILDCARD) ==> {
          val support = getMimeTypeSupport.applyOrElse(mimeType, {
            throw new RuntimeException(s"Unsupported response mime-type ($mimeType).")
          })
          status APPLY (support.serialize(tpe) APPLY REF("answer"))
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

  final def generatePackageObject(implicit ctx: GeneratorContext): Seq[SyntaxString] = {

    val helpers = generateHelpers

    if (helpers.nonEmpty) {
      val imports = EmptyTree inPackage ctx.controllerPackageName
      val objectName = ctx.controllerPackageName.split('.').last
      val objectTree = OBJECTDEF(objectName).withFlags(Flags.PACKAGE) := BLOCK(helpers)
      SyntaxString(objectName, treeToString(imports), treeToString(objectTree)) :: Nil
    } else {
      Nil
    }

  }

}
