package play.boilerplate.generators

import eu.unicredit.swagger.generators.{SharedServerClientCode, SyntaxString}
import io.swagger.models.{Operation, Swagger}
import security.SecurityProvider
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._

final class PlayServiceGenerator(securityProvider: SecurityProvider)
  extends SharedGeneratorCode
    with SharedServerClientCode {

  def serviceNameFromFileName(fn: String): String = {
    objectNameFromFileName(fn, "Service")
  }

  def servicePackageName(packageName: String): String = {
    packageName + ".service"
  }

  def generateImports(packageName: String, codeProvidedPackage: String): Seq[Tree] = {
    Seq(
      IMPORT(packageName, "_"),
      IMPORT("scala.concurrent", "Future")
    ) ++
      Seq(codeProvidedPackage).filterNot(_.isEmpty).map(IMPORT(_, "_")) ++
      securityProvider.serviceImports
  }

  def generate(fileName: String, packageName: String, codeProvidedPackage: String): Iterable[SyntaxString] = {

    parseSwagger(fileName).map { swagger =>

      val serviceName = serviceNameFromFileName(fileName)

      val serviceImports = BLOCK {
        generateImports(packageName, codeProvidedPackage)
      } inPackage servicePackageName(packageName)

      val completePaths = Option(swagger.getPaths).map(_.keySet().toSeq).getOrElse(Nil)

      val methods = completePaths.flatMap(p => composeMethods(swagger, p))

      if (methods.nonEmpty) {

        val serviceTree = TRAITDEF(serviceName) := BLOCK {
          IMPORT(serviceName, "_") +:
            methods :+
            generateOrErrorMethod
        }

        val companionTree = generateCompanionObject(serviceName, swagger, completePaths)

        SyntaxString(serviceName, treeToString(serviceImports), treeToString(serviceTree) + "\n\n" + treeToString(companionTree)) :: Nil

      } else {
        Nil
      }

    }.getOrElse(Nil)

  }

  def composeMethods(swagger: Swagger, p: String): Seq[Tree] = {

    Option(swagger.getPath(p))
      .map { path =>

        val operations = getAllOperations(path)

        for {
          op <- operations.values.toSeq
        } yield generateMethod(op)

      }
      .getOrElse(Nil)

  }

  def generateMethod(operation: Operation): Tree = {

    val methodName = operation.getOperationId

    val bodyParams = generateParamsFromBody(operation.getParameters)
    val methodParams = getMethodParamas(operation.getParameters)
    val securityParams = SecurityProvider.parseAction(operation, securityProvider).securityParams

/*
    val response = getOkRespType(operation) getOrElse {
      throw new Exception(s"Cannot determine Ok result type for $methodName")
    }
*/

    val methodType = TYPE_REF(getOperationResponseTraitName(methodName))

    val methodTree = DEF(methodName, FUTURE(methodType))
      .withParams(bodyParams.values ++ methodParams.values ++ securityParams.values)
      .empty

    methodTree.withDoc(
      s"""${Option(operation.getDescription).getOrElse("")}
         |
         """.stripMargin
    )

  }

  def generateOrErrorMethod: Tree = {

    val operationId: ValDef = PARAM("operationId", StringClass.toType).tree
    val cause      : ValDef = PARAM("cause", RootClass.newClass("Throwable")).tree

    val methodTree = DEF("onError", FUTURE(StringClass.toType))
      .withParams(operationId, cause)
      .empty

    methodTree.withDoc(
      "Error handler",
      DocTag.Param("operationId", "Operation where error was occurred"),
      DocTag.Param("cause"      , "An occurred error")
    )

  }

  def generateCompanionObject(objectName: String, swagger: Swagger, paths: Seq[String]): Tree = {

    val operations = paths.flatMap(p => Option(swagger.getPath(p))).flatMap(getAllOperations)
    val operationResults = operations.flatMap {
      case (_, operation) => generateOperationResults(operation)
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

    OBJECTDEF(objectName) := BLOCK {
      operationResults.flatMap(_.tree) ++ UnexpectedResultDef
    }

  }

  case class Responses(traitName: String, tree: Seq[Tree], withDefault: Boolean)

  def generateOperationResults(operation: Operation): Option[Responses] = {

    Option(operation.getOperationId).map { operationId =>

      val traitName = getOperationResponseTraitName(operationId)

      val sealedTrait = TRAITDEF(traitName).withFlags(Flags.SEALED).empty

      val operations = getOperationResponses(operation)

      val responses = for (response <- operations) yield {
        val className = response.className(operationId)
        val params = response.body.map(tpe => PARAM("body", tpe).tree).toSeq ++
          Some(PARAM("status", IntClass) := LIT(response.code)).filter(_ => response.isDefault).toSeq
        if (params.isEmpty) {
          CASEOBJECTDEF(className).withParents(traitName).empty
        } else {
          CASECLASSDEF(className).withParams(params).withParents(traitName).withFlags(Flags.FINAL).empty
        }
      }

      Responses(traitName, sealedTrait +: responses, operations.exists(_.isDefault))

    }

  }

}
