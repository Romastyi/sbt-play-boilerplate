package play.boilerplate.generators

import eu.unicredit.swagger.StringUtils
import eu.unicredit.swagger.generators.SharedServerClientCode
import io.swagger.models.parameters.{BodyParameter, Parameter}
import io.swagger.models.{Operation, Path, Swagger}
import io.swagger.parser.SwaggerParser
import treehugger.forest._
import treehuggerDSL._

import scala.collection.JavaConversions._

trait SharedGeneratorCode { this: SharedServerClientCode =>

  final def IDENTITY(tpe: Type): Tree = REF("identity")
  final def FUTURE(tpe: Type)  : Type = TYPE_REF("Future") TYPE_OF tpe

  /*
   * final case class UnexpectedResult(body: String = "", code: Int = 200) extends ...
   */
  val UnexpectedResult = TypeName("UnexpectedResult")

  val DefaultMimeTypes: Seq[String] = Seq(
    "application/json"
  )

  def SupportedMimeTypes: Seq[String] = DefaultMimeTypes

  def operationConsumes(swagger: Swagger, operation: Operation): Seq[String] = {
    val globalConsumes = Option(swagger.getConsumes).map(_.toIndexedSeq).getOrElse(Nil)
    Option(operation.getConsumes).map(_.toIndexedSeq).getOrElse(globalConsumes)
      .filter { mimeType =>
        if (SupportedMimeTypes.exists(mimeType.startsWith)) {
          true
        } else {
          println(s"WARNING - consumes mime-type '$mimeType' is not supported!")
          false
        }
      }
  }

  def operationProduces(swagger: Swagger, operation: Operation): Seq[String] = {
    val globalProduces = Option(swagger.getProduces).map(_.toIndexedSeq).getOrElse(Nil)
    Option(operation.getProduces).map(_.toIndexedSeq).getOrElse(globalProduces)
      .filter { mimeType =>
        if (SupportedMimeTypes.exists(mimeType.startsWith)) {
          true
        } else {
          println(s"WARNING - produces mime-type '$mimeType' is not supported!")
          false
        }
      }
  }

  def generateParamsFromBody(params: Seq[Parameter]): Map[String, ValDef] = {
    params.collect {
      case bp: BodyParameter =>
        val tpe = noOptParamType(bp)
        val param: ValDef = PARAM(bp.getName, tpe).tree
        bp.getName -> param
    }.toMap
  }

  def getAllOperations(path: Path): Map[String, Operation] = {
    Seq(
      Option(path.getDelete) map ("DELETE" -> _),
      Option(path.getGet   ) map ("GET"    -> _),
      Option(path.getPost  ) map ("POST"   -> _),
      Option(path.getPut   ) map ("PUT"    -> _)
    ).flatten.toMap
  }

  private val codeStatus: Map[String, String] = Map(
    "200" -> "Ok",
    "201" -> "Created",
    "202" -> "Accepted",
    "203" -> "NonAuthoritativeInformation",
    "204" -> "NoContent",
    "205" -> "ResetContent",
    "206" -> "PartialContent",
    "207" -> "MultiStatus",
    "400" -> "BadRequest",
    "500" -> "InternalServerError"
  )

  case class OperationResponse(response: String, body: Option[Type], status: Option[String]) {

    val isDefault: Boolean = response == "default"

    val code: Int = if (isDefault) 200 else response.toInt

    def className(operationId: String): String = {
      StringUtils.toCamelCase(operationId) + StringUtils.toCamelCase(status.getOrElse(response))
    }

  }

  def getOperationResponseTraitName(operationId: String): String = {
    StringUtils.toCamelCase(operationId) + "Response"
  }

  def getOperationResponses(operation: Operation): Seq[OperationResponse] = {
    val responses = Option(operation.getResponses).map(_.toSeq).getOrElse(Nil)
    for ((code, response) <- responses) yield {
      OperationResponse(code, Option(response.getSchema).map(noOptPropType), codeStatus.get(code))
    }
  }

  def parseSwagger(fileName: String): Option[Swagger] = {
    Option(new SwaggerParser().read(fileName))
  }

}