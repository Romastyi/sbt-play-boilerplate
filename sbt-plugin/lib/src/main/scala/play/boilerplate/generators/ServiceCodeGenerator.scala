package play.boilerplate.generators

import play.boilerplate.parser.model._

class ServiceCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def securityImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Import] = {
    getSecurityProviderOfSchema(schema).flatMap(_.serviceImports)
  }

  def generateImports(schema: Schema)(implicit ctx: GeneratorContext): Seq[Tree] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF("scala.language"), "higherKinds")
    ) ++
      tracesImports ++
      securityImports(schema) ++
      ctx.settings.codeProvidedPackages.filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
  }

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val serviceImports = BLOCK {
      generateImports(schema)
    } inPackage ctx.settings.servicePackageName

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(path, operation)(ctx.addCurrentPath(operation.operationId).setInService(true))

    if (methods.nonEmpty) {

      val serviceTree = TRAITDEF(ctx.settings.serviceClassName).withTypeParams(F_TYPEVAR) :=
        BLOCK {
          IMPORT(REF(ctx.settings.serviceClassName), "_") +: filterNonEmptyTree(
            methods.map(_.tree).toIndexedSeq
          )
        }

      val companionTree = OBJECTDEF(ctx.settings.serviceClassName) := BLOCK {
        (VAL(serviceNameValName) := LIT(ctx.settings.serviceName)).withDoc("Service name: " + ctx.settings.serviceName) +:
          (generateResponseClasses(schema)(ctx.setInService(true)) ++ methods.flatMap(_.additionalDef))
      }

      SourceCodeFile(
        packageName = ctx.settings.servicePackageName,
        className = ctx.settings.serviceClassName,
        header = treeToString(serviceImports),
        impl = treeToString(serviceTree, EmptyTree, companionTree)
      ) :: Nil

    } else {
      Nil
    }

  }

  case class Method(tree: Tree, additionalDef: Seq[Tree])

  def generateMethod(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val methodParams = getBodyParameters(path, operation) ++ getMethodParameters(path, operation)
    val actionSecurity = getSecurityProvider(operation).getActionSecurity(operation.security.toIndexedSeq)
    val securityParams = actionSecurity.securityParamsDef

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val methodTree = methodDefinition(operation.operationId, F_OF_TYPE(methodType), methodParams.map(_._2), securityParams.toIndexedSeq).empty

    val paramDocs = methodParams.map(_._2.doc) ++
      actionSecurity.securityDocs.map { case (param, description) =>
        DocTag.Param(param, description)
      }
    val tree = methodTree.withDoc(
      Seq(operation.description.getOrElse("") + "\n "),
      paramDocs: _ *
    )

    val additionalDef = methodParams.flatMap(_._2.additionalDef)

    Method(tree, filterNonEmptyTree(additionalDef))

  }

  def generateResponseClasses(schema: Schema)(implicit ctx: GeneratorContext): Seq[Tree] = {

    val models = schema.definitions
    val operationResults = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateOperationResults(operation, models)(ctx.addCurrentPath(operation.operationId))

    val traits = operationResults.map(_.traitName)

    val UnexpectedResultDef = {
      CASECLASSDEF(UnexpectedResultClassName)
        .withParams(
          PARAM("body", StringClass).empty,
          PARAM("code", IntClass) := LIT(500),
          PARAM("contentType", StringClass) := LIT(MIME_TYPE_TEXT)
        )
        .withParents(traits)
        .withFlags(Flags.FINAL) := BLOCK(headersMethodDef.withFlags(Flags.OVERRIDE) := NIL)
    }.withDoc(
        Seq("Response for unexpected result of request."),
        DocTag.Param("body", "Response body."),
        DocTag.Param("code", "Response code (default: 500)."),
        DocTag.Param("contentType", "Response Content-Type (default: text/plain).")
      )

    operationResults.flatMap(_.tree) ++ Seq(UnexpectedResultDef)

  }

  case class Responses(traitName: String, tree: Seq[Tree])

  private def headersMethodDef: DefTreeStart = {
    DEF("headers", TYPE_SEQ(TYPE_TUPLE(StringClass, StringClass)))
  }

  def generateOperationResults(operation: Operation, models: Map[String, Model])
                              (implicit ctx: GeneratorContext): Responses = {

    val traitName = getOperationResponseTraitName(operation.operationId)

    val sealedTrait = (TRAITDEF(traitName).withFlags(Flags.SEALED) := BLOCK {
      headersMethodDef.empty
    }).withDoc(
      Seq(s"Response to operation '${operation.operationId}'.")
    )

    val hasOk = operation.responses.keys.exists {
      case StatusResponse(code) if HttpStatus.codeIsOk(code) => true
      case _ => false
    }

    val responses = for ((code, response) <- operation.responses.toSeq) yield {
      val className = getResponseClassName(operation.operationId, code)
      val bodyType  = getResponseBodyType(response)
      val fullParamsList = bodyType.map(body => ResponseParam(
        headerName = None,
        paramName = "body",
        paramDef = PARAM("body", body.tpe).tree,
        paramDoc = DocTag.Param("body", response.schema.flatMap(_.description).getOrElse("")),
        isOptional = false
      )).toSeq ++ {
        code match {
          case DefaultResponse =>
            val defaultCode = if (hasOk) 500 else 200
            Seq(ResponseParam(
              headerName = None,
              paramName = "code",
              paramDef = PARAM("code", IntClass) := LIT(defaultCode),
              paramDoc = DocTag.Param("code", s"Response code (default: $defaultCode)"),
              isOptional = false
            ))
          case _ =>
            Nil
        }
      } ++ getResponseParameters(response)
      val headersList = fullParamsList.collect { case ResponseParam(Some(headerName), paramName, _, _, isOptional) =>
        val paramVal = if (isTraceIdHeaderName(headerName)) traceIdValRef(REF(paramName)) else REF(paramName)
        PAIR(LIT(headerName), if (isOptional) paramVal else SOME(paramVal))
      }
      val headerMethodImpl = if (headersList.isEmpty) {
        headersMethodDef.withFlags(Flags.OVERRIDE) := NIL
      } else {
        headersMethodDef.withFlags(Flags.OVERRIDE) := BLOCK(
          LIST(headersList) INFIX "collect" APPLY BLOCK {
            CASE(PAREN(ID("key"), SOME(ID("value")))) ==>
              PAIR(REF("key"), REF("value"))
          }
        )
      }
      val classDef = if (fullParamsList.isEmpty) {
        CASEOBJECTDEF(className).withParents(traitName)
      } else {
        CASECLASSDEF(className).withParams(fullParamsList.map(_.paramDef)).withParents(traitName).withFlags(Flags.FINAL)
      }
      val classTree = (classDef := BLOCK {
        headerMethodImpl
      }).withDoc(
        Seq(response.description.getOrElse("")),
        fullParamsList.map(_.paramDoc): _ *
      )
      bodyType.map(_.definitions).getOrElse(Nil) :+ classTree
    }

    Responses(traitName, sealedTrait +: responses.flatten.toIndexedSeq)

  }

}
