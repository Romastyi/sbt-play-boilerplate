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
      securityImports(schema) ++
      ctx.settings.loggerProvider.imports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
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

      val serviceTree = TRAITDEF(ctx.settings.serviceClassName)
        .withTypeParams(F_TYPEVAR)
        .withParents(ctx.settings.loggerProvider.parents)
        .withSelf("self", ctx.settings.loggerProvider.selfTypes: _ *) :=
        BLOCK {
          IMPORT(REF(ctx.settings.serviceClassName), "_") +: filterNonEmptyTree(
            ctx.settings.loggerProvider.loggerDefs ++
            methods.map(_.tree).toIndexedSeq
          )
        }

      val companionTree = OBJECTDEF(ctx.settings.serviceClassName) := BLOCK {
        generateResponseClasses(schema)(ctx.setInService(true)) ++ methods.flatMap(_.additionalDef)
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

    val bodyParams = getBodyParameters(path, operation)
    val methodParams = getMethodParameters(path, operation)
    val actionSecurity = getSecurityProvider(operation).getActionSecurity(operation.security.toIndexedSeq)
    val securityParams = actionSecurity.securityParamsDef

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val methodTree = DEF(operation.operationId, F_OF_TYPE(methodType))
      .withParams(bodyParams.map(_._2.valDef) ++ methodParams.map(_._2.valDef) ++ securityParams)
      .empty

    val paramDocs = (bodyParams.map(_._2) ++ methodParams.map(_._2)).map(_.doc) ++
      actionSecurity.securityDocs.map { case (param, description) =>
        DocTag.Param(param, description)
      }
    val tree = methodTree.withDoc(
      Seq(operation.description.getOrElse("") + "\n "),
      paramDocs: _ *
    )

    val additionalDef = bodyParams.flatMap(_._2.additionalDef) ++
      methodParams.flatMap(_._2.additionalDef)

    Method(tree, filterNonEmptyTree(additionalDef))

  }

  def generateResponseClasses(schema: Schema)(implicit ctx: GeneratorContext): Seq[Tree] = {

    val models = schema.definitions
    val operationResults = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateOperationResults(operation, models)(ctx.addCurrentPath(operation.operationId))

    val traits = operationResults.map(_.traitName)

    val UnexpectedResultDef = CASECLASSDEF(UnexpectedResult)
      .withParams(
        PARAM("body", StringClass).empty,
        PARAM("code", IntClass) := LIT(500),
        PARAM("contentType", StringClass) := LIT(MIME_TYPE_TEXT)
      )
      .withParents(traits)
      .withFlags(Flags.FINAL)
      .empty
      .withDoc(
        Seq("Response for unexpected result of request."),
        DocTag.Param("body", "Response body."),
        DocTag.Param("code", "Response code (default: 500)."),
        DocTag.Param("contentType", "Response Content-Type (default: text/plain).")
      )

    operationResults.flatMap(_.tree) ++ Seq(UnexpectedResultDef)

  }

  case class Responses(traitName: String, tree: Seq[Tree])
  case class ResponseParam(paramDef: ValDef, paramDoc: DocTag)

  def generateOperationResults(operation: Operation, models: Map[String, Model])
                              (implicit ctx: GeneratorContext): Responses = {

    val traitName = getOperationResponseTraitName(operation.operationId)

    val sealedTrait = TRAITDEF(traitName).withFlags(Flags.SEALED).empty.withDoc(
      Seq(s"Response to operation '${operation.operationId}'.")
    )

    val hasOk = operation.responses.keys.exists {
      case StatusResponse(code) if HttpStatus.codeIsOk(code) => true
      case _ => false
    }

    val responses = for ((code, response) <- operation.responses.toSeq) yield {
      val className = getResponseClassName(operation.operationId, code)
      val bodyType  = getResponseBodyType(response)
      val params = bodyType.map(body => ResponseParam(
        paramDef = PARAM("body", body.tpe).tree,
        paramDoc = DocTag.Param("body", response.schema.flatMap(_.description).getOrElse(""))
      )).toSeq ++ {
        code match {
          case DefaultResponse =>
            val defaultCode = if (hasOk) 500 else 200
            Seq(ResponseParam(
              paramDef = PARAM("code", IntClass) := LIT(defaultCode),
              paramDoc = DocTag.Param("code", s"Response code (default: $defaultCode)")
            ))
          case _ =>
            Nil
        }
      }
      val classDef = if (params.isEmpty) {
        CASEOBJECTDEF(className).withParents(traitName).empty
      } else {
        CASECLASSDEF(className).withParams(params.map(_.paramDef)).withParents(traitName).withFlags(Flags.FINAL).empty
      }
      bodyType.map(_.definitions).getOrElse(Nil) :+ classDef.withDoc(
        Seq(response.description.getOrElse("")),
        params.map(_.paramDoc): _ *
      )
    }

    Responses(traitName, sealedTrait +: responses.flatten.toIndexedSeq)

  }

}
