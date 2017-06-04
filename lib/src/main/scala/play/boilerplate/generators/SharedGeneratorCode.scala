package play.boilerplate.generators

import eu.unicredit.swagger.generators.SharedServerClientCode
import io.swagger.models.parameters._
import io.swagger.models.{Model, Operation, Path, Swagger}
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

  case class MethodParam(valDef: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree])

  def generateMethodParams(className: String, params: Seq[Parameter], models: Map[String, Model]): Map[String, MethodParam] = {
    params
      .filter {
        case _: PathParameter   => true
        case _: QueryParameter  => true
        case _: HeaderParameter => true
        case _: BodyParameter   => false
        case x =>
          println(s"Unmanaged parameter type for parameter ${x.getName}, please contact the developer to implement it XD")
          false
      }
      .sortBy { //the order must be verified...
        case _: HeaderParameter => 1
        case _: PathParameter   => 2
        case _: QueryParameter  => 3
        // other subtypes have been removed already
      }
      .map { p =>
        val PropType(tpe, defs) = fullParamType(className, p, models)
        (p.getName, MethodParam(PARAM(p.getName, tpe).tree, defs.map(_.definition), generateParamsImplicits(p, defs)))
      }
      .toMap
  }

  def generateParamsImplicits(p: Parameter, implicits: Seq[PropDefs]): Seq[Tree] = {
    p match {
      case _: PathParameter =>
        implicits.map(_.pathBindable)
      case _: QueryParameter =>
        implicits.map(_.queryBindable)
      case _ =>
        Nil
    }
  }

  def generateParamsFromBody(methodName: String, params: Seq[Parameter], models: Map[String, Model]): Map[String, MethodParam] = {
    params.collect {
      case bp: BodyParameter =>
        val PropType(tpe, defs) = fullNoOptParamType(methodName, bp, models)
        bp.getName -> MethodParam(PARAM(bp.getName, tpe).tree, defs.map(_.definition), Nil)
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
    // Success
    "200" -> "Ok",
    "201" -> "Created",
    "202" -> "Accepted",
    "203" -> "NonAuthoritativeInformation",
    "204" -> "NoContent",
    "205" -> "ResetContent",
    "206" -> "PartialContent",
    "207" -> "MultiStatus",
    // Request error
    "400" -> "BadRequest",
    "401" -> "Unauthorized",
    "402" -> "PaymentRequired",
    "403" -> "Forbidden",
    "404" -> "NotFound",
    "405" -> "MethodNotAllowed",
    "406" -> "NotAcceptable",
    "408" -> "RequestTimeout",
    "409" -> "Conflict",
    "410" -> "Gone",
    "415" -> "UnsupportedMediaType",
    // Server error
    "500" -> "InternalServerError"
  )

  case class OperationResponse(response: String, body: Option[Type], status: Option[String]) {

    val isDefault: Boolean = response == "default"

    val code: Int = if (isDefault) 200 else response.toInt

    def className(operationId: String): String = {
      operationId.capitalize + status.getOrElse(response).capitalize
    }

  }

  def getOperationResponseTraitName(operationId: String): String = {
    operationId.capitalize + "Response"
  }

  def getOperationResponses(operation: Operation, models: Map[String, Model]): Seq[OperationResponse] = {
    val responses = Option(operation.getResponses).map(_.toSeq).getOrElse(Nil)
    for ((code, response) <- responses) yield {
      val tpe = Option(response.getSchema).map(noOptPropType(_, models).tpe)
      OperationResponse(code, tpe, codeStatus.get(code))
    }
  }

}