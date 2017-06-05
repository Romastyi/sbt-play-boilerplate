package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.parser.model._

class PlayServiceGeneratorParser(schema: Schema) {

  import GeneratorUtils._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def generateImports(implicit ctx: GeneratorContext): Seq[Tree] = {
    Seq(
      IMPORT(ctx.basePackageName, "_"),
      IMPORT("scala.concurrent", "Future")
    ) ++
      Seq(ctx.codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_")) ++
      ctx.securityProvider.serviceImports
  }

  def generate(implicit ctx: GeneratorContext): Iterable[SyntaxString] = {

    val serviceImports = BLOCK {
      generateImports
    } inPackage ctx.servicePackageName

    val methods = for {
      path <- schema.paths
      op <- path.operations.values
    } yield generateMethod(path, op)(ctx.addCurrentPath(op.operationId).setInService(true))

    if (methods.nonEmpty) {

      val serviceTree = TRAITDEF(ctx.serviceClassName) := BLOCK {
        IMPORT(ctx.serviceClassName, "_") +:
          methods.map(_.tree).toIndexedSeq :+
          generateOrErrorMethod
      }

      val companionTree = OBJECTDEF(ctx.serviceClassName) := BLOCK {
        generateResponseClasses(ctx.setInService(true)) ++ methods.flatMap(_.additionalDef)
      }

      SyntaxString(ctx.serviceClassName, treeToString(serviceImports), treeToString(serviceTree, companionTree)) :: Nil

    } else {
      Nil
    }

  }

  case class Method(tree: Tree, additionalDef: Seq[Tree])

  def generateMethod(path: Path, operation: Operation)(implicit ctx: GeneratorContext): Method = {

    val bodyParams = getBodyParameters(path, operation)
    val methodParams = getMethodParameters(path, operation)
    val securityParams = ctx.securityProvider.getActionSecurity(operation.security.toIndexedSeq).securityParams

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
      .withParams(operationId, cause)
      .empty

    methodTree.withDoc(
      "Error handler",
      DocTag.Param("operationId", "Operation where error was occurred"),
      DocTag.Param("cause"      , "An occurred error")
    )

  }

  def generateResponseClasses(implicit ctx: GeneratorContext): Seq[Tree] = {

    val models = schema.definitions
    val operations = schema.paths.flatMap(_.operations)
    val operationResults = operations.map {
      case (_, operation) => generateOperationResults(operation, models)
    }

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
