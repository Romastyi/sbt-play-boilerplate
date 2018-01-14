package play.boilerplate.generators

import play.boilerplate.generators.support.{DefinitionContext, DefinitionsSupport, TypeSupport}
import play.boilerplate.parser.model._

object GeneratorUtils extends StringUtils with DefinitionsSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  final def IDENTITY(tpe: Type): Tree = REF("identity")
  final def FUTURE(tpe: Type)  : Type = TYPE_REF("Future") TYPE_OF tpe

  final val MIME_TYPE_JSON = "application/json"
  final val MIME_TYPE_TEXT = "text/plain"

  final val ACTION_ANYCONTENT: Type = TYPE_REF("Action") TYPE_OF "AnyContent"
  final val ACTION_EMPTY     : Type = TYPE_REF("Action") TYPE_OF UnitClass

  final val PARSER_ANYCONTENT: Tree = REF("parse") DOT "anyContent"
  final val PARSER_EMPTY     : Tree = REF("parse") DOT "empty"

  final val REQUEST_AS_JSON: Tree = REF("request") DOT "body" DOT "asJson"
  final val REQUEST_AS_TEXT: Tree = REF("request") DOT "body" DOT "asText"
  final val REQUEST_EMPTY  : Tree = SOME(UNIT)

  final def JSON_TO_TYPE(tpe: Type)(ident: Ident): Tree = ident DOT "as" APPLYTYPE tpe
  final def TYPE_TO_JSON(tpe: Type)(ident: Ident): Tree = REF("Json") DOT "toJson" APPLYTYPE tpe APPLY ident

  case class MimeTypeSupport(mimeType: String,
                             requestBody: Tree,
                             deserialize: Type => Ident => Tree,
                             serialize: Type => Ident => Tree)

  def getMimeTypeSupport: PartialFunction[String, MimeTypeSupport] = {
    case MIME_TYPE_JSON => MimeTypeSupport(MIME_TYPE_JSON, REQUEST_AS_JSON, JSON_TO_TYPE, TYPE_TO_JSON)
    case MIME_TYPE_TEXT => MimeTypeSupport(MIME_TYPE_TEXT, REQUEST_AS_TEXT, tpe => ident => tpe APPLY ident, _ => ident => ident DOT "toString()")
  }

  case class MethodParam(valDef: ValDef, fullQualified: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree], defaultValue: Option[Tree])

  def getBodyParameters(path: Path, operation: Operation)
                       (implicit ctx: GeneratorContext): Map[String, MethodParam] = {
    (path.parameters ++ operation.parameters).collect {
      case param: BodyParameter =>
        val paramName = decapitalize(param.name)
        val support = getTypeSupport(param.ref)
        val valDef = PARAM(paramName, support.tpe).empty
        val fullQualified = PARAM(paramName, support.fullQualified).empty
        paramName -> MethodParam(valDef, fullQualified, support.definitions, Nil, None)
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
        val paramName = decapitalize(param.name)
        val defaultValue = getDefaultValue(param)
        val support = getTypeSupport(param.ref, DefinitionContext.inline.copy(canBeOption = defaultValue.isEmpty))
        val valDef = defaultValue match {
          case Some(default) => PARAM(paramName, support.tpe) := support.constructor(default)
          case None => PARAM(paramName, support.tpe).empty
        }
        val fullQualified = PARAM(paramName, support.fullQualified).empty
        paramName -> MethodParam(valDef, fullQualified, support.definitions, getParamImplicits(param, support), defaultValue.map(support.constructor.apply))
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

  def getDefaultValue(definition: Definition): Option[Literal] = {
    definition.baseDef match {
      case d: WithDefault[_] =>
        d.default.map(LIT.apply)
      case _ =>
        None
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
      IMPORT(REF("play.api.mvc"), "_"),
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
