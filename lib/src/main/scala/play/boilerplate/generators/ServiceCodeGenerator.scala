package play.boilerplate.generators

import play.boilerplate.parser.model._

class ServiceCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Tree] = {
    Seq(
      IMPORT(ctx.settings.modelPackageName, "_"),
      IMPORT("scala.concurrent", "Future")
    ) ++
      ctx.settings.securityProvider.serviceImports ++
      Seq(ctx.settings.codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_"))
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

      val serviceTree = TRAITDEF(ctx.settings.serviceClassName) := BLOCK {
        IMPORT(ctx.settings.serviceClassName, "_") +:
          methods.map(_.tree).toIndexedSeq :+
          generateOrErrorMethod
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
    val securityParams = ctx.settings.securityProvider.getActionSecurity(operation.security.toIndexedSeq).securityParams

    val methodType = TYPE_REF(getOperationResponseTraitName(operation.operationId))

    val methodTree = DEF(operation.operationId, FUTURE(methodType))
      .withParams(bodyParams.values.map(_.valDef) ++ methodParams.values.map(_.valDef) ++ securityParams.values)
      .empty

    val tree = methodTree.withDoc(
      s"""${operation.description.getOrElse("")}
         |
         """.stripMargin
    )

    val additionalDef = bodyParams.values.flatMap(_.additionalDef) ++
      methodParams.values.flatMap(_.additionalDef)

    Method(tree, filterNonEmptyTree(additionalDef))

  }

  def generateOrErrorMethod: Tree = {

    val operationId: ValDef = PARAM("operationId", StringClass.toType).tree
    val cause      : ValDef = PARAM("cause", RootClass.newClass("Throwable")).tree

    val methodTree = DEF("onError", FUTURE(StringClass))
      .withParams(operationId, cause) :=
      BLOCK(
        VAL("message") := INFIX_CHAIN("+",
          INTERP(StringContext_s, LIT("Unexpected error (operationId: "), REF("operationId"), LIT("): ")),
          REF("cause") DOT "getMessage"
        ),
        REF("Future") DOT "successful" APPLY REF("message")
      )

    methodTree.withDoc(
      "Error handler",
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

    val traits = operationResults.filterNot(_.withDefault).map(_.traitName)

    val UnexpectedResultDef = if (traits.nonEmpty) {
      Some(CASECLASSDEF(UnexpectedResult)
        .withParams(
          PARAM("body", StringClass) := LIT(""),
          PARAM("status", IntClass) := LIT(200)
        )
        .withParents(traits)
        .withFlags(Flags.FINAL)
        .empty
      )
    } else {
      None
    }

    operationResults.flatMap(_.tree) ++ UnexpectedResultDef

  }

  case class Responses(traitName: String, tree: Seq[Tree], withDefault: Boolean)

  def generateOperationResults(operation: Operation, models: Map[String, Model])
                              (implicit ctx: GeneratorContext): Responses = {

    val traitName = getOperationResponseTraitName(operation.operationId)

    val sealedTrait = TRAITDEF(traitName).withFlags(Flags.SEALED).empty

    val withDefault = operation.responses.keySet(DefaultResponse)

    val responses = for ((code, response) <- operation.responses.toSeq) yield {
      val className = getResponseClassName(operation.operationId, code)
      val bodyType = response.schema.map(
        body => getTypeSupport(body)(ctx.addCurrentPath(operation.operationId, "body"))
      )
      val params = bodyType.map(body => PARAM("body", body.tpe).tree).toSeq ++ {
        code match {
          case DefaultResponse =>
            Seq(PARAM("status", IntClass) := LIT(200))
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

    Responses(traitName, sealedTrait +: responses.flatten.toIndexedSeq, withDefault)

  }

}
