package play.boilerplate.generators

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._
import treehugger.forest._
import definitions._
import treehuggerDSL._

class ControllerCodeGenerator extends CodeGenerator {

  import GeneratorUtils._

  private val traceLoggerValName = "traceLogger"
  private val traceLoggerValRef = REF(traceLoggerValName)

  private val requestValName ="request"
  private val requestValRef = REF(requestValName)

  def securityImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Import] = {
    getSecurityProviderOfSchema(schema).flatMap(_.controllerImports)
  }

  def securityDependencies(schema: Schema)(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    getSecurityProviderOfSchema(schema).flatMap(_.controllerDependencies)
  }

  def securityParents(schema: Schema)(implicit ctx: GeneratorContext): Seq[Type] = {
    getSecurityProviderOfSchema(schema).flatMap(_.controllerParents)
  }

  def securitySelfTypes(schema: Schema)(implicit ctx: GeneratorContext): Seq[Type] = {
    getSecurityProviderOfSchema(schema).flatMap(_.controllerSelfTypes)
  }

  def generateImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF(ctx.settings.jsonImportPrefix), "_"),
      IMPORT(REF(ctx.settings.servicePackageName), ctx.settings.serviceClassName),
      IMPORT(REF(ctx.settings.serviceClassName), "_"),
      IMPORT(REF("play.api.data.validation"), "_"),
      IMPORT(REF("play.api.libs.json"), "_"),
      IMPORT(REF("play.api.libs.functional.syntax"), "_"),
      IMPORT(REF("play.api.mvc"), "_"),
      IMPORT(REF("play.boilerplate.api.server.dsl"), "_"),
      IMPORT(REF("scala.concurrent"), "ExecutionContext", "Future")
    ) ++
      tracesImports ++
      securityImports(schema) ++
      ctx.settings.injectionProvider.imports ++
      ctx.settings.loggerProvider.imports ++
      ctx.settings.codeProvidedPackages.filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
  }

  def dependencies(schema: Schema)(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(
      InjectionProvider.Dependency("service", TYPE_REF(ctx.settings.serviceClassName) TYPE_OF TYPE_REF("Future")),
      InjectionProvider.Dependency("ec", TYPE_REF("ExecutionContext"), isImplicit = true)
    ) ++ securityDependencies(schema)
  }

  def baseControllerType: Type = TYPE_REF("Controller")

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(schema, path, operation)(ctx.addCurrentPath(operation.operationId).setInController(true))

    if (methods.nonEmpty) {

      val controllerImports = BLOCK {
        generateImports(schema)
      } inPackage ctx.settings.controllerPackageName

      val companionItems = distinctTreeByName(filterNonEmptyTree(methods.flatMap(_.implicits)))

      val (companionObj, importCompanion) = if (companionItems.nonEmpty) {
        val objDef = OBJECTDEF(ctx.settings.controllerClassName) := BLOCK {
          companionItems
        }
        (objDef, IMPORT(REF(ctx.settings.controllerClassName), "_"))
      } else {
        (EmptyTree, EmptyTree)
      }
      val importCompat = IMPORT(REF("Compat"), "_")

      val parents = Seq(baseControllerType, TYPE_REF("ControllerMixins")) ++
        securityParents(schema) ++
        ctx.settings.loggerProvider.parents

      val classDef = CLASSDEF(ctx.settings.controllerClassName)
        .withParents(parents)
        .withSelf("self", securitySelfTypes(schema): _ *) :=
        BLOCK {
          filterNonEmptyTree(
            Seq(importCompanion, importCompat) ++
              ctx.settings.loggerProvider.loggerDefs ++
              Seq(VAL(traceLoggerValName) := ctx.settings.loggerProvider.controllerTraceLoggerDef) ++
              methods.map(_.tree).toIndexedSeq
          )
        }

      // --- DI
      val controllerTree = ctx.settings.injectionProvider.classDefModifier(classDef, dependencies(schema))

      SourceCodeFile(
        packageName = ctx.settings.controllerPackageName,
        className = ctx.settings.controllerClassName,
        header = treeToString(controllerImports),
        impl = treeToString(companionObj) + "\n\n" + controllerTree
      ) :: Nil

    } else {
      Nil
    }

  }

  private def paramGetter(param: Parameter, accessor: Tree): (String, ValDef) = {
    val paramName = getParameterIdentifier(param)
    val valDef = VAL(paramName) := {
      if (isOptional(param)) accessor
      else accessor DOT "getOrElse" APPLY THROW(IllegalArgumentExceptionClass, s"""Value of parameter "${param.name}" is not specified""")
    }
    paramName -> valDef
  }

  case class Method(tree: Tree, implicits: Seq[Tree])

  def generateMethod(schema: Schema, path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val methodName = operation.operationId
    val bodyContentType = getBodyContentType(schema, path, operation)
    val supportedConsumes = operation.consumes.filter(supportedMimeTypes)

    if (bodyContentType != NoContent && supportedConsumes.isEmpty) {
      throw new RuntimeException(s"There is no supported consumers for operation (operationId: ${operation.operationId}).")
    }

    val bodyValues = generateValuesFromBody(operation, supportedConsumes.flatMap(mimeType => getMimeTypeSupport.lift(mimeType)))
    val methodParams = getMethodParameters(path, operation, withHeaders = false, withFormData = false)
    val fullParamsList = getFullParametersList(path, operation)
    val headerParams = fullParamsList.collect {
      case param: HeaderParameter if !isTraceIdHeaderParameter(param)=>
        paramGetter(param, requestValRef DOT "headers" DOT "get" APPLY LIT(param.name))
    }.distinctBy(_._1)
    val formDataParams = fullParamsList.collect {
      case param: FormParameter =>
        val accessor = param.baseDef match {
          case _: FileDefinition =>"file"
          case _: StringDefinition => if (bodyContentType == FormUrlencoded) "formValue" else "dataPart"
          case _ => throw new RuntimeException(s"Unsupported type for formData parameter (operationId: ${operation.operationId}, parameter: ${param.name}).")
        }
        paramGetter(param, requestValRef DOT "body" DOT accessor APPLY LIT(param.name))
    }.distinctBy(_._1)

    val actionSecurity = getSecurityProvider(operation).getActionSecurity(operation.security.toIndexedSeq)

    val (actionType, parser) = if (bodyContentType == NoContent) {
      (ACTION_EMPTY, PARSER_EMPTY)
    } else {
      (ACTION_ANYCONTENT, PARSER_ANYCONTENT)
    }

    val methodCall = REF("service") DOT methodName APPLY {
      (bodyValues.map(_.valName) ++
        methodParams.map(_._1) ++
        formDataParams.map(_._1) ++
        headerParams.map(_._1) ++
        actionSecurity.securityValues.map(_._1)).map(REF(_))
    }

    val answerMethod = generateAnswer(operation)

    val ANSWER_WITH_EXCEPTION_HANDLE = methodCall INFIX "collect" APPLY {
      answerMethod.tree
    } INFIX "recover" APPLY BLOCK {
      CASE(REF("cause") withType RootClass.newClass("Throwable")) ==> BLOCK(
        traceLoggerValRef DOT "logError" APPLY (LIT(operation.operationId), LIT("Unhandled error."), REF("cause")),
        REF("InternalServerError") APPLY (REF("cause") DOT "getMessage")
      )
    }

    val ANSWER_WITH_ACCEPT_CHECK = if (ctx.settings.strictAcceptHeaderCheck) {
      generateAcceptCheck(operation, ANSWER_WITH_EXCEPTION_HANDLE)
    } else ANSWER_WITH_EXCEPTION_HANDLE

    val tracerVal = VAL(tracerValName, tracerType).withFlags(Flags.IMPLICIT) := {
      ctx.settings.effectiveTraceIdHeader match {
        case Some(headerName) =>
          requestValRef DOT "headers" DOT "get" APPLY LIT(headerName) DOT "map" APPLY tracerCtor DOT "getOrElse" APPLY tracerRandom
        case None =>
          tracerRandom
      }
    }

    val methodValues = actionSecurity.securityValues.map(_._2) ++
      headerParams.map(_._2) ++
      formDataParams.map(_._2) ++
      bodyValues.map(_.valDef)

    val BODY_WITH_EXCEPTION_HANDLE =
      BLOCK {
        filterNonEmptyTree(Seq(
          VAL("r").withFlags(Flags.IMPLICIT) := requestValRef,
          tracerVal,
          traceLoggerValRef DOT "logRequest" APPLY (LIT(operation.operationId), requestValRef)
        )) ++
          methodValues :+
          ANSWER_WITH_ACCEPT_CHECK
      }

    val methodTree =
      DEF(methodName, actionType) withParams methodParams.map(_._2.paramDef) := BLOCK {
        actionSecurity.actionMethod(parser) APPLY {
          LAMBDA(PARAM(requestValName).empty) ==> BODY_WITH_EXCEPTION_HANDLE
        }
      }

    val tree = methodTree.withDoc(
      operation.description.getOrElse("")
    )

    val implicits = bodyValues.flatMap(_.implicits) ++ methodParams.flatMap(_._2.implicits)

    Method(tree, answerMethod.implicits ++ implicits)

  }

  private def traceLogResponse(operation: Operation, status: Tree, body: Tree, contentType: Tree): Tree = {
    traceLoggerValRef DOT "logResponse" APPLY (LIT(operation.operationId), status, body, contentType)
  }

  case class BodyValue(valName: String, valDef: ValDef, implicits: Seq[Tree])

  final def generateValuesFromBody(operation: Operation, consumes: Iterable[MimeTypeSupport])
                                  (implicit ctx: GeneratorContext): Seq[BodyValue] = {

    if (consumes.isEmpty) {
      Nil
    } else {
      operation.parameters.collect {
        case bp: BodyParameter =>
          val support = getTypeSupport(bp.ref)
          val tpe = support.tpe
          val resolvers = consumes.map { support =>
            support.requestBody INFIX "map" APPLY
              LAMBDA(PARAM("body").tree) ==> BLOCK(
                support.deserialize(tpe)(REF("body"))
              )
          }
          val valName = getParameterIdentifier(bp)
          val valDef = VAL(valName) :=
            INFIX_CHAIN("orElse", resolvers) INFIX "getOrElse" APPLY BLOCK {
              THROW(IllegalArgumentExceptionClass, "Invalid body format")
            }
          BodyValue(valName, valDef, support.jsonReads ++ support.jsonWrites)
      }.distinctBy(_.valName).toIndexedSeq
    }

  }

  private val responseValName = "response"
  private val responseValRef = REF(responseValName)

  private def generateResponseCase(className: String, unapplyParams: Seq[Ident], code: Ident => Tree): CaseDef = {
    if (unapplyParams.nonEmpty) {
      CASE((REF(className) UNAPPLY unapplyParams) withBinder responseValName) ==> code(responseValRef)
    } else {
      CASE(REF(className)) ==> code(REF(className))
    }
  }

  private val contentValName = "content"
  private val contentValRef = REF(contentValName)
  private val contentValId = ID(contentValName)

  private val codeValName = "code"
  private val codeValRef = REF(codeValName)
  private val codeValId = ID(codeValName)

  private val contentTypeValName = "contentType"
  private val contentTypeValRef = REF(contentTypeValName)
  private val contentTypeValId = ID(contentTypeValName)

  private val responseBodyValName = "responseBody"
  private val responseBodyValRef = REF(responseBodyValName)

  private val statusValName = "status"
  private val statusValRef = REF(statusValName)

  private def composeResponseStatus(responseVal: Ident, status: Tree): Tree = {
    status DOT "withHeaders" APPLY SEQARG(responseVal DOT "headers")
  }

  private def composeResponseWithMimeType(responseVal: Ident, status: Tree, content: Ident, mimeType: Tree): Tree = {
    status APPLY content DOT "withHeaders" APPLY SEQARG(responseVal DOT "headers") DOT "as" APPLY mimeType
  }

  private def getMimeTypeConst(mimeType: String): Tree = LIT(mimeType + "; charset=utf-8")

  private def composeResponseWithMimeType(responseVal: Ident, status: Tree, mimeType: String): Tree = {
    composeResponseWithMimeType(responseVal, status, responseBodyValRef, getMimeTypeConst(mimeType))
  }

  private def STATUS_WITH_CODE(code: Tree): Tree = REF("Status") APPLY code

  final def generateAnswer(operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val supportedProduces = operation.produces.filterNot(_ == MIME_TYPE_JSON).flatMap(
      mimeType => getMimeTypeSupport.lift(mimeType)
    )

    val cases = for ((responseCode, response) <- operation.responses) yield {
      val className = getResponseClassName(operation.operationId, responseCode)
      val bodyType  = getResponseBodyType(response)
      val params    = getResponseParameters(response)
      val statusCode = Some(responseCode).flatMap {
        case DefaultResponse => None
        case StatusResponse(code) => Some(code)
      }
      val (traceCode, status) = statusCode.map { code =>
        HttpStatus.findStatusByCode(code).map {
          name => (LIT(code.toString + " " + name): Tree, REF(name): Tree)
        }.getOrElse {
          (LIT(code), STATUS_WITH_CODE(LIT(code)))
        }
      }.getOrElse {
        (codeValRef, STATUS_WITH_CODE(codeValRef))
      }
      val unapplyParams = params.map(_ => WILDCARD)
      val tree = (bodyType.map(_.tpe), Some(IntClass).filter(_ => responseCode == DefaultResponse)) match {
        case (Some(body), Some(_)) =>
          generateResponseCase(
            className,
            contentValId +: codeValId +: unapplyParams,
            responseVal => generateResponse(operation, responseVal, status, traceCode, body, supportedProduces, BLOCK(
              VAL(responseBodyValName) := TYPE_TO_JSON(body)(contentValRef),
              VAL(statusValName) := composeResponseWithMimeType(responseVal, status, MIME_TYPE_JSON),
              traceLogResponse(operation, statusValRef, responseBodyValRef, getMimeTypeConst(MIME_TYPE_JSON)),
              statusValRef
            ))
          )
        case (Some(body), None) =>
          generateResponseCase(
            className,
            contentValId +: unapplyParams,
            responseVal => generateResponse(operation, responseVal, status, traceCode, body, supportedProduces, BLOCK(
              VAL(responseBodyValName) := TYPE_TO_JSON(body)(contentValRef),
              VAL(statusValName) := composeResponseWithMimeType(responseVal, status, MIME_TYPE_JSON),
              traceLogResponse(operation, statusValRef, responseBodyValRef, getMimeTypeConst(MIME_TYPE_JSON)),
              statusValRef
            ))
          )
        case (None, Some(_)) =>
          generateResponseCase(
            className,
            codeValId +: unapplyParams,
            responseVal => generateResponse(operation, responseVal, status, traceCode, UnitClass, Nil, BLOCK(
              VAL(statusValName) := composeResponseStatus(responseVal, status),
              traceLogResponse(operation, statusValRef, LIT(""), LIT("")),
              statusValRef
            ))
          )
        case (None, None) =>
          generateResponseCase(
            className,
            unapplyParams,
            responseVal => generateResponse(operation, responseVal, status, traceCode, UnitClass, Nil, BLOCK(
              VAL(statusValName) := composeResponseStatus(responseVal, status),
              traceLogResponse(operation, statusValRef, LIT(""), LIT("")),
              statusValRef
            ))
          )
      }
      Method(tree, bodyType.map(s => s.jsonReads ++ s.jsonWrites).getOrElse(Nil))
    }

    val UnexpectedResultCase = generateResponseCase(
      UnexpectedResultClassName,
      Seq(contentValId, codeValId, contentTypeValId),
      responseVal => BLOCK(
        VAL(statusValName) := composeResponseWithMimeType(responseVal, STATUS_WITH_CODE(codeValRef), contentValRef, contentTypeValRef),
        traceLogResponse(operation, statusValRef, contentValRef, contentTypeValRef),
        statusValRef
      )
    )

    val tree = BLOCK {
      cases.map(_.tree) ++ Seq(UnexpectedResultCase)
    }

    Method(tree, cases.flatMap(_.implicits))

  }

  private def acceptsMimeType(mimeType: String): Tree = {
    REF("acceptsMimeType") APPLY LIT(mimeType)
  }

  private def generateAcceptCheck(operation: Operation, tree: Tree)(implicit ctx: GeneratorContext): Tree = {
    val supportedMimeTypes = operation.produces.flatMap(
      mimeType => getMimeTypeSupport.lift(mimeType)
    ).map(_.mimeType)
    if (supportedMimeTypes.nonEmpty) {
      IF (INFIX_CHAIN("||", supportedMimeTypes.map(acceptsMimeType))) THEN BLOCK {
        tree
      } ELSE BLOCK {
        THROW(IllegalArgumentExceptionClass, s"Invalid 'Accept' header value. Must be one of:\n${supportedMimeTypes.mkString("\n")}")
      }
    } else {
      tree
    }
  }

  private def generateResponse(operation: Operation,
                               responseVal: Ident,
                               status: Tree,
                               traceCode: Tree,
                               tpe: Type,
                               produces: Iterable[MimeTypeSupport],
                               default: Tree)
                              (implicit ctx: GeneratorContext): Tree = {
    if (produces.isEmpty) {
      default
    } else {
      scala.Function.chain(
        produces.toIndexedSeq.map { support =>
          new (Tree => Tree) {
            override def apply(v1: Tree): Tree = {
              IF (acceptsMimeType(support.mimeType)) THEN BLOCK(
                VAL(responseBodyValName) := support.serialize(tpe)(contentValRef),
                VAL(statusValName) := composeResponseWithMimeType(responseVal, status, support.mimeType),
                traceLogResponse(operation, statusValRef, responseBodyValRef, getMimeTypeConst(support.mimeType)),
                statusValRef
              ) ELSE {
                v1
              }
            }
          }
        }
      ) apply default
    }
  }

}

object InjectedControllerCodeGenerator extends ControllerCodeGenerator {
  override def baseControllerType: Type = TYPE_REF("InjectedController")
}