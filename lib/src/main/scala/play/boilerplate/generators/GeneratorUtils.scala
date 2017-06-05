package play.boilerplate.generators

import play.boilerplate.generators.support.{DefinitionsSupport, TypeSupport}
import play.boilerplate.parser.model._

object GeneratorUtils extends StringUtils with DefinitionsSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  final def IDENTITY(tpe: Type): Tree = REF("identity")
  final def FUTURE(tpe: Type)  : Type = TYPE_REF("Future") TYPE_OF tpe

  final val MIME_TYPE_JSON = "application/json"

  final val ACTION_ANYCONTENT: Type = TYPE_REF("Action") TYPE_OF "AnyContent"
  final val ACTION_EMPTY     : Type = TYPE_REF("Action") TYPE_OF UnitClass

  final val PARSER_ANYCONTENT: Tree = REF("parse") DOT "anyContent"
  final val PARSER_EMPTY     : Tree = REF("parse") DOT "empty"

  final val REQUEST_AS_JSON: Tree = REF("request") DOT "body" DOT "asJson"
  final val REQUEST_EMPTY  : Tree = SOME(UNIT)

  final def JSON_TO_TYPE(tpe: Type): Tree = WILDCARD DOT "as" APPLYTYPE tpe
  final def TYPE_TO_JSON(tpe: Type): Tree = REF("Json") DOT "toJson" APPLYTYPE tpe

  case class MimeTypeSupport(requestBody: Tree,
                             deserialize: Type => Tree,
                             serialize: Type => Tree)

  def getMimeTypeSupport: PartialFunction[String, MimeTypeSupport] = {
    case MIME_TYPE_JSON => MimeTypeSupport(REQUEST_AS_JSON, JSON_TO_TYPE, TYPE_TO_JSON)
  }

  case class MethodParam(valDef: ValDef, fullQualified: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree])

  def getBodyParameters(path: Path, operation: Operation)
                       (implicit ctx: GeneratorContext): Map[String, MethodParam] = {
    (path.parameters ++ operation.parameters).collect {
      case param: BodyParameter =>
        val support = getTypeSupport(param.ref)
        val valDef = PARAM(param.name, support.tpe).empty
        val fullQualified = PARAM(param.name, support.fullQualified).empty
        param.name -> MethodParam(valDef, fullQualified, support.definitions, Nil)
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
        val valDef = PARAM(param.name, support.tpe).empty
        val fullQualified = PARAM(param.name, support.fullQualified).empty
        param.name -> MethodParam(valDef, fullQualified, support.definitions, getParamImplicits(param, support))
      }
      .toMap
  }

  def getParamImplicits(param: Parameter, support: TypeSupport): Seq[Tree] = {
    param match {
      case _: PathParameter =>
        support.pathBindable
      case _: QueryParameter =>
        support.queryBindable
      case _ =>
        Nil
    }
  }

  private val statusByCode = Map(
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
  )

  /*
   * final case class UnexpectedResult(body: String = "", code: Int = 200) extends ...
   */
  val UnexpectedResult = TypeName("UnexpectedResult")

  def getStatusByCode(code: Int): Option[String] = statusByCode.get(code)

  def getOperationResponseTraitName(operationId: String): String = {
    operationId.capitalize + "Response"
  }

  def getResponseClassName(operationId: String, responseCode: ResponseCode): String = {
    operationId.capitalize + (responseCode match {
      case DefaultResponse => "Default"
      case StatusResponse(status) => getStatusByCode(status).getOrElse(status.toString)
    }).capitalize
  }

  def generateAcceptMatcher: Tree = {
    val tpe = (OptionClass APPLYTYPE StringClass.toType).tpe
    BLOCK(
      IMPORT("play.api.mvc", "_"),
      CASECLASSDEF("AcceptMatcher") withParams PARAM("mimeType", StringClass.toType).tree := BLOCK {
        DEF("unapply", tpe) withParams PARAM("arg", TYPE_REF("RequestHeader")).tree := BLOCK {
          IF((REF("arg") DOT "acceptedTypes" DOT "nonEmpty") AND (REF("arg") DOT "accepts" APPLY REF("mimeType"))) THEN BLOCK {
            SOME(REF("mimeType"))
          } ELSE BLOCK {
            NONE
          }
        }
      }
    )
  }

  def filterNonEmptyTree(trees: Seq[Tree]): Seq[Tree] = trees.filterNot(_ == EmptyTree)

  def doClientUrl(basePath: String, path: Iterable[PathPart]): String = {
    val parts = path.collect {
      case StaticPart(str) => str
      case ParamPart(name) => "$" + name
    }.toSeq
    cleanDuplicateSlash((basePath +: parts).mkString("/"))
  }

  def doRoutesUrl(basePath: String, path: Iterable[PathPart], operation: Operation): String = {
    val parts = path.collect {
      case StaticPart(str) =>
        str
      case ParamPart(name) =>
        val param = operation.parameters.find(_.name == name).map(_.baseDef).getOrElse {
          throw new RuntimeException(s"Url path parameter '$name' not found for operation (${operation.operationId}).")
        }
        param match {
          case _: IntegerDefinition | _: LongDefinition => "$" + name + "<[0-9]+>"
          case _ => ":" + name
        }
    }.toSeq
    cleanDuplicateSlash((basePath +: parts).mkString("/"))
  }

}
