package play.boilerplate.generators

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.parser.model._

class ClientCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  /* VARIABLES NAMES */

  private val selfValName = "self"
  private val selfValRef = REF(selfValName)

  private val handlerValName = "handler"
  private val handlerValRef = REF(handlerValName)

  private val wsValName = "ws"
  private val wsValRef = REF(wsValName)

  private val locatorValName = "locator"
  private val locatorValRef = REF(locatorValName)

  private val requestValName = "request"
  private val requestValRef = REF(requestValName)

  private val responseValName = "response"
  private val responseValRef = REF(responseValName)

  private val bodyValName = "body"
  private val bodyValRef = REF(bodyValName)
  private val bodyValId = ID(bodyValName)

  private val codeValName = "code"
  private val codeValRef = REF(codeValName)
  private val codeValId = ID(codeValName)

  private val contentTypeValName = "contentType"
  private val contentTypeValRef= REF(contentTypeValName)
  private val contentTypeValId = ID(contentTypeValName)

  private val causeValName = "cause"
  private val causeValRef = REF(causeValName)
  private val causeValId = ID(causeValName)

  /* VARIABLES NAMES */

  private def contentTypeOf(responseVal: Ident): Tree = REF("getContentType") APPLY responseVal

  def securityImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Import] = {
    getSecurityProviderOfSchema(schema).flatMap(_.serviceImports)
  }

  def generateImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Import] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF(ctx.settings.jsonImportPrefix), "_"),
      IMPORT(REF(ctx.settings.servicePackageName), ctx.settings.serviceClassName),
      IMPORT(REF(ctx.settings.serviceClassName), "_"),
      IMPORT(REF("play.api.libs.json"), "_"),
      IMPORT(REF("play.api.libs.functional.syntax"), "_"),
      IMPORT(REF("play.boilerplate.api.client.dsl"), "_"),
      IMPORT(REF("play.boilerplate.api.client.dsl.Compat"), "_"),
      IMPORT(REF("scala.concurrent"), "ExecutionContext", "Future")
    ) ++
      securityImports(schema) ++
      ctx.settings.injectionProvider.imports ++
      ctx.settings.loggerProvider.imports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
  }

  def dependencies(implicit ctx: GeneratorContext): Seq[InjectionProvider.Dependency] = {
    Seq(
      InjectionProvider.Dependency(handlerValName, TYPE_REF("RequestHandler") TYPE_OF TYPE_REF(ctx.settings.clientClassName), Some(REF("RequestHandler") DOT "default" APPLYTYPE TYPE_REF(ctx.settings.clientClassName))),
      InjectionProvider.Dependency(wsValName, TYPE_REF("WSClient"), isImplicit = true),
      InjectionProvider.Dependency(locatorValName, TYPE_REF("ServiceLocator"), isImplicit = true),
      InjectionProvider.Dependency("ec", TYPE_REF("ExecutionContext"), isImplicit = true)
    )
  }

  def credentialsClassName(implicit ctx: GeneratorContext): String = {
    objectNameFromFileName(ctx.settings.fileName, "Credentials")
  }

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(schema, path, operation)(ctx.addCurrentPath(operation.operationId).setInClient(true))

    if (methods.nonEmpty) {

      val clientImports = BLOCK {
        generateImports(schema)
      } inPackage ctx.settings.clientPackageName

      val parents = Seq(
        TYPE_REF(ctx.settings.serviceClassName) TYPE_OF TYPE_REF("Future"),
        TYPE_REF("ClientHelpers")
      ) ++
        ctx.settings.loggerProvider.parents

      val methodImplicits = distinctTreeByName(filterNonEmptyTree(methods.flatMap(_.implicits)))
      val methodDefinitions = filterNonEmptyTree(methods.map(_.tree))

      val classDef = CLASSDEF(ctx.settings.clientClassName)
        .withParents(parents)
        .withSelf(selfValName) :=
        BLOCK {
          methodImplicits ++ methodDefinitions ++ Seq(
            generateOrErrorMethod
          )
        }

      // --- DI
      val clientTree = ctx.settings.injectionProvider.classDefModifier(classDef, dependencies)

      val credentialsClass = generateCredentialsClass(schema)

      SourceCodeFile(
        packageName = ctx.settings.clientPackageName,
        className = ctx.settings.clientClassName,
        header = treeToString(clientImports),
        impl = treeToString(credentialsClass: _ *) + "\n\n" + clientTree
      ) :: Nil

    } else {
      Nil
    }

  }

  def composeClientUrl(basePath: String, path: Path, operation: Operation): ValDef = {

    val basePathParts = Seq(basePath.dropWhile(_ == '/')).filterNot(_.isEmpty).map(LIT.apply)

    val parts = path.pathParts.collect {
      case StaticPart(str) => LIT(str)
      case ParamPart(name) => REF("_render_path_param") APPLY (LIT(name), REF(name))
    }.toSeq

    val urlParts = (REF("uri") +: (basePathParts ++ parts)).foldLeft(List.empty[Tree]) { case (acc, term) =>
      if (acc.isEmpty) acc :+ term else acc :+ LIT("/") :+ term
    }

    val urlParams: Seq[Tree] = getFullParametersList(path, operation).collect {
      case param: QueryParameter => LIT(param.name) INFIX ("->", REF(param.name))
    }

    val baseUrl = INTERP(StringContext_s, urlParts: _ *)

    val tree = if (urlParams.isEmpty) {
      baseUrl
    } else {
      baseUrl INFIX("+", REF("_render_url_params") APPLY (urlParams: _*))
    }

    VAL("url") := tree

  }

  case class BodyContent(params: Seq[(String, MethodParam)], serialization: Tree)

  private def getBodyContent(path: Path, operation: Operation, bodyContentType: BodyContentType)
                            (implicit ctx: GeneratorContext): BodyContent = bodyContentType match {
    case NoContent =>
      BodyContent(
        params = Nil,
        serialization = operation.httpMethod match {
          case HttpMethod.Put | HttpMethod.Post | HttpMethod.Patch =>
            REF("Array") DOT "emptyByteArray"
          case _ =>
            EmptyTree
        }
      )
    case SimpleContent =>
      val params = getBodyParameters(path, operation)
      BodyContent(
        params = params,
        serialization = params.headOption.map { case (name, _) =>
          REF("Json") DOT "toJson" APPLY REF(name)
        }.getOrElse(EmptyTree)
      )
    case MultipartFormData =>
      val formDataParams = getFullParametersList(path, operation).collect {
        case formData: FormParameter => formData
      }
      val parts: Seq[Tree] = formDataParams.flatMap { param =>
        val paramKey = LIT(param.name)
        val paramRef = REF(getParameterIdentifier(param))
        val paramVal = if (isOptional(param)) paramRef else SOME(paramRef)
        param.baseDef match {
          case _: StringDefinition =>
            Some(REF("dataPart") APPLY (paramKey, paramVal))
          case _: FileDefinition =>
            Some(REF("filePart") APPLY (paramKey, LIT(MIME_TYPE_APPLICATION_OCTET_STREAM), paramVal))
          case _ =>
            throw new RuntimeException(s"Unsupported type for formData parameter (operationId: ${operation.operationId}, parameter: ${param.name}).")
        }
      }
      BodyContent(
        params = Nil,
        serialization = {
          REF("multipartFormData") APPLY LIST(parts)
        }
      )
    case FormUrlencoded =>
      val params = getFullParametersList(path, operation).collect {
        case formData: FormParameter => formData
      }.filter { param => param.baseDef match {
        case _: FileDefinition => false
        case _: StringDefinition => true
        case _ => throw new RuntimeException(s"Unsupported type for formData parameter (operationId: ${operation.operationId}, parameter: ${param.name}).")
      }}.map { param =>
        val paramName = getParameterIdentifier(param)
        PAIR(LIT(param.name), if (isOptional(param)) REF(paramName) DOT "toSeq" else SEQ(REF(paramName)))
      }
      BodyContent(
        params = Nil,
        serialization = MAKE_MAP(params)
      )
  }

  case class Method(tree: Tree, implicits: Seq[Tree])

  def generateMethod(schema: Schema, path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val bodyContentType = getBodyContentType(schema, path, operation)
    val bodyContent = getBodyContent(path, operation, bodyContentType)
    val methodParams = getMethodParameters(path, operation)
    val actionSecurity = getSecurityProvider(operation).getActionSecurity(operation.security.toIndexedSeq)
    val securityParams = actionSecurity.securityParamsDef

    val headerParams: Seq[Tree] = getFullParametersList(path, operation).collect {
      case param: HeaderParameter if !isTraceIdHeaderParameter(param) =>
        val paramName = getParameterIdentifier(param)
        val ref = if (isOptional(param)) REF(paramName) else SOME(REF(paramName))
        LIT(param.name) INFIX ("->", ref)
    } ++ ctx.settings.effectiveTraceIdHeader.toIndexedSeq.map { headerName =>
      LIT(headerName) INFIX ("->", SOME(traceIdValRef))
    }

    val urlValDef = composeClientUrl(schema.basePath, path, operation)

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val opType = operation.httpMethod.toString.toLowerCase
    val wsRequestWithAccept = requestValRef DOT "addHttpHeaders" APPLY (LIT("Accept") INFIX ("->", LIT(MIME_TYPE_JSON)))
    val wsRequestWithHeaderParams = if (headerParams.isEmpty) {
      wsRequestWithAccept
    } else {
      wsRequestWithAccept DOT "addHttpHeaders" APPLY SEQARG(REF("_render_header_params") APPLY (headerParams: _*))
    }

    val credentials = {
      val params = actionSecurity.securityParams
        .distinctBy(_._1)
        .map {
          case (ident, _) => REF(ident) := REF(ident)
        }
      if (params.nonEmpty) {
        REF(credentialsClassName) APPLY(params: _ *)
      } else {
        REF("NoCredentials")
      }
    }
    val beforeRequest = handlerValRef DOT "beforeRequest" APPLY(
      LIT(operation.operationId),
      wsValRef DOT "url" APPLY REF("url"),
      credentials
    )
    val responses = generateResponses(responseValRef, operation)

    val ERROR = BLOCK(
      handlerValRef DOT "onError" APPLY(LIT(operation.operationId), causeValRef, credentials),
      selfValRef DOT "onError" APPLY(LIT(operation.operationId), causeValRef)
    )

    val methodTree = DEF(operation.operationId, FUTURE(methodType))
      .withFlags(Flags.OVERRIDE)
      .withParams(bodyContent.params.map(_._2.valDef) ++ methodParams.map(_._2.valDef) ++ securityParams) :=
      BLOCK {
        locatorValRef DOT "doServiceCall" APPLY(serviceNameValRef, LIT(operation.operationId)) APPLY {
          LAMBDA(PARAM("uri").tree) ==> BLOCK(
            urlValDef,
            VAL("f") := FOR(
              VALFROM(requestValName) := beforeRequest,
              VALFROM(responseValName) := wsRequestWithHeaderParams DOT opType APPLY bodyContent.serialization,
              VAL(WILDCARD) := handlerValRef DOT "onSuccess" APPLY(LIT(operation.operationId), responseValRef, credentials),
              VAL("result") := responses.tree
            ) YIELD REF("result"),
            REF("f") DOT "recoverWith" APPLY BLOCK {
              CASE(causeValRef withType RootClass.newClass("Throwable")) ==> ERROR
            }
          )
        }
      }

    val paramDocs = (bodyContent.params.map(_._2) ++ methodParams.map(_._2)).map(_.doc) ++
      actionSecurity.securityDocs.map { case (param, description) =>
        DocTag.Param(param, description)
      }
    val tree = methodTree.withDoc(
      Seq(operation.description.getOrElse("") + "\n "),
      paramDocs: _ *
    )

    val implicits = bodyContent.params.flatMap(_._2.implicits) ++ methodParams.flatMap(_._2.implicits)

    Method(tree, responses.implicits ++ implicits)

  }

  def generateResponses(responseVal: Ident, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val responses = operation.responses.toSeq.sortBy {
      case (DefaultResponse, _) => 2
      case _ => 1
    }

    val cases = for ((responseCode, response) <- responses) yield {
      val className = getResponseClassName(operation.operationId, responseCode)
      val bodyType  = getResponseBodyType(response)
      val bodyParam = bodyType.map { body =>
        REF("parseResponseAsJson") APPLYTYPE body.tpe APPLY responseVal
      }
      val (pattern, params) = responseCode match {
        case DefaultResponse =>
          (codeValId, bodyParam.toIndexedSeq :+ codeValRef)
        case StatusResponse(code) =>
          (LIT(code), bodyParam.toIndexedSeq)
      }
      val headerParams = getResponseParameters(response).collect {
        case ResponseParam(Some(headerName), _, _, _, isOptional) =>
          val extractor = responseValRef DOT "header" APPLY LIT(headerName)
          if (isOptional) extractor else extractor DOT "getOrElse" APPLY LIT("")
      }
      val fullParamsList = params ++ headerParams
      val caseTree = CASE(pattern) ==> {
        if (fullParamsList.nonEmpty) REF(className) APPLY fullParamsList
        else REF(className)
      }
      (caseTree, bodyType.map(s => s.jsonReads ++ s.jsonWrites).getOrElse(Nil))
    }

    val UnexpectedResultCase = if (operation.responses.keySet(DefaultResponse)) {
      None
    } else {
      Some(
        CASE(codeValId) ==> {
          REF(UnexpectedResultClassName) APPLY(responseVal DOT "body", codeValRef, contentTypeOf(responseVal))
        }
      )
    }

    val tree = responseVal DOT "status" MATCH {
      cases.map(_._1) ++ UnexpectedResultCase
    }

    Method(tree, cases.flatMap(_._2))

  }

  def generateOrErrorMethod(implicit ctx: GeneratorContext): Tree = {

    val operationId: ValDef = PARAM("operationId", StringClass.toType).tree
    val exception  : ValDef = PARAM("ex", RootClass.newClass("Throwable")).tree

    val exceptionsCase = REF("ex") MATCH(
      CASE(REF("JsonParsingError") UNAPPLY(causeValId, bodyValId, codeValId, contentTypeValId)) ==> BLOCK(
        {
          val message = INTERP(StringContext_s,
            LIT("JSON parsing error (operationId: "), REF("operationId"), LIT("): "),
            causeValRef DOT "getMessage", LIT("\nOriginal body: "), bodyValRef
          )
          ctx.settings.loggerProvider.error(message, causeValRef)
        },
        REF("Future") DOT "successful" APPLY (REF(UnexpectedResultClassName) APPLY (bodyValRef, codeValRef, contentTypeValRef))
      ),
      CASE(REF("JsonValidationError") UNAPPLY(causeValId, bodyValId, codeValId, contentTypeValId)) ==> BLOCK(
        {
          val message = INTERP(StringContext_s,
            LIT("JSON validation error (operationId: "), REF("operationId"), LIT("): "),
            causeValRef DOT "getMessage", LIT("\nOriginal body: "), bodyValRef
          )
          ctx.settings.loggerProvider.error(message, causeValRef)
        },
        REF("Future") DOT "successful" APPLY (REF(UnexpectedResultClassName) APPLY (bodyValRef DOT "toString", codeValRef, contentTypeValRef))
      ),
      CASE(REF("UnexpectedResponseError") UNAPPLY(causeValId, bodyValId, codeValId, contentTypeValId)) ==> BLOCK(
        {
          val message = INTERP(StringContext_s,
            LIT("Unexpected response (operationId: "), REF("operationId"), LIT("): "),
            codeValRef, LIT(" "), bodyValRef
          )
          ctx.settings.loggerProvider.error(message, causeValRef)
        },
        REF("Future") DOT "successful" APPLY (REF(UnexpectedResultClassName) APPLY (bodyValRef, codeValRef, contentTypeValRef))
      ),
      CASE(causeValRef) ==> BLOCK(
        {
          val message = INTERP(StringContext_s,
            LIT("Unexpected error (operationId: "), REF("operationId"), LIT("): "),
            causeValRef DOT "getMessage"
          )
          ctx.settings.loggerProvider.error(message, causeValRef)
        },
        REF("Future") DOT "failed" APPLY causeValRef
      )
    )

    val methodTree = DEF("onError", FUTURE(TYPE_REF(UnexpectedResultClassName)))
      .withParams(operationId, exception) :=
      BLOCK(
        exceptionsCase
      )

    methodTree.withDoc(
      "Error handler\n ",
      DocTag.Param("operationId", "Operation where error was occurred"),
      DocTag.Param("ex"         , "An occurred error")
    )

  }

  def generateCredentialsClass(schema: Schema)(implicit ctx: GeneratorContext): Seq[Tree] = {

    val securityParams = schema.paths.flatMap(_.operations.values)
      .foldLeft(Seq.empty[(String, Type)]) { case (acc, op) =>
        acc ++ getSecurityProvider(op).getActionSecurity(op.security.toIndexedSeq).securityParams
      }.distinctBy(_._1)

    if (securityParams.nonEmpty) {
      val params = securityParams.map {
        case (name, tpe) => PARAM(name, tpe).tree
      }
      Seq(
        CASECLASSDEF(credentialsClassName)
          .withFlags(Flags.FINAL)
          .withParams(params: _ *)
          .withParents(TYPE_REF("Credentials") TYPE_OF TYPE_REF(ctx.settings.clientClassName)).empty
      )
    } else {
      Nil
    }

  }

}
