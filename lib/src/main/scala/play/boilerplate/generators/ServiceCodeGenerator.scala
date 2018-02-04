package play.boilerplate.generators

import play.boilerplate.parser.model._

class ServiceCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Tree] = {
    Seq(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF("scala.concurrent"), "Future")
    ) ++
      ctx.settings.securityProvider.serviceImports ++
      ctx.settings.loggerProvider.imports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(pkg => IMPORT(REF(pkg), "_"))
  }

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val serviceImports = BLOCK {
      generateImports
    } inPackage ctx.settings.servicePackageName

    val methods = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateMethod(path, operation)(ctx.addCurrentPath(operation.operationId).setInService(true))

    if (methods.nonEmpty) {

      val serviceTree = TRAITDEF(ctx.settings.serviceClassName)
        .withParents(ctx.settings.loggerProvider.parents)
        .withSelf("self", ctx.settings.loggerProvider.selfTypes: _ *) :=
        BLOCK {
          IMPORT(REF(ctx.settings.serviceClassName), "_") +: filterNonEmptyTree(
            ctx.settings.loggerProvider.loggerDefs ++
            methods.map(_.tree).toIndexedSeq :+
            generateOrErrorMethod
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
    val actionSecurity = ctx.settings.securityProvider.getActionSecurity(operation.security.toIndexedSeq)
    val securityParams = actionSecurity.securityParamsDef

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val methodTree = DEF(operation.operationId, FUTURE(methodType))
      .withParams(bodyParams.values.map(_.valDef) ++ methodParams.values.map(_.valDef) ++ securityParams)
      .empty

    val paramDocs = (bodyParams.values ++ methodParams.values).map(_.doc) ++
      actionSecurity.securityDocs.map { case (param, description) =>
        DocTag.Param(param, description)
      }
    val tree = methodTree.withDoc(
      Seq(operation.description.getOrElse("") + "\n "),
      paramDocs.toIndexedSeq: _ *
    )

    val additionalDef = bodyParams.values.flatMap(_.additionalDef) ++
      methodParams.values.flatMap(_.additionalDef)

    Method(tree, filterNonEmptyTree(additionalDef))

  }

  def generateOrErrorMethod(implicit ctx: GeneratorContext): Tree = {

    val operationId: ValDef = PARAM("operationId", StringClass.toType).tree
    val cause      : ValDef = PARAM("cause", RootClass.newClass("Throwable")).tree

    val methodTree = DEF("onError", FUTURE(TYPE_REF(UnexpectedResult)))
      .withParams(operationId, cause) :=
      BLOCK(
        VAL("message") := INFIX_CHAIN("+",
          INTERP(StringContext_s, LIT("Unexpected error (operationId: "), REF("operationId"), LIT("): ")),
          REF("cause") DOT "getMessage"
        ),
        ctx.settings.loggerProvider.error(REF("message"), REF("cause")),
        REF("Future") DOT "successful" APPLY (REF(UnexpectedResult) APPLY (REF("body") := REF("message")))
      )

    methodTree.withDoc(
      "Error handler\n ",
      DocTag.Param("operationId", "Operation where error was occurred"),
      DocTag.Param("cause"      , "An occurred error")
    )

  }

  def generateResponseClasses(schema: Schema)(implicit ctx: GeneratorContext): Seq[Tree] = {

    val models = schema.definitions
    val operationResults = for {
      path <- schema.paths
      (_, operation) <- path.operations.toSeq.sortBy(_._1)
    } yield generateOperationResults(operation, models)

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

    operationResults.flatMap(_.tree) ++ Seq(UnexpectedResultDef)

  }

  case class Responses(traitName: String, tree: Seq[Tree])

  def generateOperationResults(operation: Operation, models: Map[String, Model])
                              (implicit ctx: GeneratorContext): Responses = {

    val traitName = getOperationResponseTraitName(operation.operationId)

    val sealedTrait = TRAITDEF(traitName).withFlags(Flags.SEALED).empty

    val hasOk = operation.responses.keys.exists {
      case StatusResponse(code) if codeIsOk(code) => true
      case _ => false
    }

    val responses = for ((code, response) <- operation.responses.toSeq) yield {
      val className = getResponseClassName(operation.operationId, code)
      val bodyType = response.schema.map(
        body => getTypeSupport(body)(ctx.addCurrentPath(operation.operationId, "body"))
      )
      val params = bodyType.map(body => PARAM("body", body.tpe).tree).toSeq ++ {
        code match {
          case DefaultResponse =>
            Seq(PARAM("code", IntClass) := LIT(if (hasOk) 500 else 200))
          case _ =>
            Nil
        }
      }
      val classDef = if (params.isEmpty) {
        CASEOBJECTDEF(className).withParents(traitName).empty
      } else {
        CASECLASSDEF(className).withParams(params).withParents(traitName).withFlags(Flags.FINAL).empty
      }
      bodyType.map(_.definitions).getOrElse(Nil) :+ classDef
    }

    Responses(traitName, sealedTrait +: responses.flatten.toIndexedSeq)

  }

}
