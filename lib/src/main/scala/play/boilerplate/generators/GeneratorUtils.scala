package play.boilerplate.generators

import play.boilerplate.generators.support.TypeSupport
import play.boilerplate.parser.model._

object GeneratorUtils extends support.DefinitionsSupport {

  import treehugger.forest._
  import treehuggerDSL._

  final def IDENTITY(tpe: Type): Tree = REF("identity")
  final def FUTURE(tpe: Type)  : Type = TYPE_REF("Future") TYPE_OF tpe

  case class MethodParam(valDef: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree])

  def getBodyParameters(path: Path, operation: Operation)
                       (implicit ctx: GeneratorContext): Map[String, MethodParam] = {
    (path.parameters ++ operation.parameters).collect {
      case param: BodyParameter =>
        val support = getTypeSupport(param.ref)
        param.name -> MethodParam(PARAM(param.name, support.tpe).tree, support.defs.map(_.definition), Nil)
    }.toMap
  }

  def getMethodParameters(path: Path, operation: Operation)
                         (implicit ctx: GeneratorContext): Map[String, MethodParam] = {
    (path.parameters ++ operation.parameters).toSeq
      .filter {
        case _: PathParameter   => true
        case _: QueryParameter  => true
        case _: HeaderParameter => true
        case _: BodyParameter   => false
        case x =>
          println(s"Unmanaged parameter type for parameter ${x.name}, please contact the developer to implement it XD")
          false
      }
      .sortBy { //the order must be verified...
        case _: HeaderParameter => 1
        case _: PathParameter   => 2
        case _: QueryParameter  => 3
        case _ => 4
      }
      .map { param =>
        val support = getTypeSupport(param.ref)
        param.name -> MethodParam(PARAM(param.name, support.tpe).tree, support.defs.map(_.definition), getParamImplicits(param, support))
      }
      .toMap
  }

  def getParamImplicits(param: Parameter, support: TypeSupport): Seq[Tree] = {
    param match {
      case _: PathParameter =>
        support.defs.map(_.pathBindable)
      case _: QueryParameter =>
        support.defs.map(_.queryBindable)
      case _ =>
        Nil
    }
  }

  private val statusByCode: Int => String = code => Map(
    // Success
    200 -> "Ok",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "NonAuthoritativeInformation",
    204 -> "NoContent",
    205 -> "ResetContent",
    206 -> "PartialContent",
    207 -> "MultiStatus",
    // Request error
    400 -> "BadRequest",
    401 -> "Unauthorized",
    402 -> "PaymentRequired",
    403 -> "Forbidden",
    404 -> "NotFound",
    405 -> "MethodNotAllowed",
    406 -> "NotAcceptable",
    408 -> "RequestTimeout",
    409 -> "Conflict",
    410 -> "Gone",
    415 -> "UnsupportedMediaType",
    // Server error
    500 -> "InternalServerError"
  ).getOrElse(code, code.toString)

  /*
   * final case class UnexpectedResult(body: String = "", code: Int = 200) extends ...
   */
  val UnexpectedResult = TypeName("UnexpectedResult")

  def getOperationResponseTraitName(operationId: String): String = {
    operationId.capitalize + "Response"
  }

  def getResponseClassName(operationId: String, responseCode: ResponseCode): String = {
    operationId.capitalize + (responseCode match {
      case DefaultResponse => "Default"
      case StatusResponse(status) => statusByCode(status)
    }).capitalize
  }

}
