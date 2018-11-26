package play.boilerplate.generators

import play.boilerplate.generators.security.SecurityProvider
import play.boilerplate.generators.support.{DefinitionContext, DefinitionsSupport, TypeSupport}
import play.boilerplate.parser.model._

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

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
  final def LOG_JSON(ident: Ident): Tree = REF("Json") DOT "stringify" APPLY ident

  case class MimeTypeSupport(mimeType: String,
                             requestBody: Tree,
                             deserialize: Type => Ident => Tree,
                             serialize: Type => Ident => Tree,
                             logContent: Ident => Tree)

  val defaultJsonSupport: MimeTypeSupport = MimeTypeSupport(MIME_TYPE_JSON, REQUEST_AS_JSON, JSON_TO_TYPE, TYPE_TO_JSON, LOG_JSON)

  def getMimeTypeSupport(implicit ctx: GeneratorContext): PartialFunction[String, MimeTypeSupport] = {
    Map(MIME_TYPE_JSON -> defaultJsonSupport) ++ ctx.settings.supportedMimeTypes
  }

  def getSecurityProviderOfSchema(schema: Schema)(implicit ctx: GeneratorContext): Seq[SecurityProvider] = {
    schema.securitySchemas.keys.toIndexedSeq.map(getSecurityProvider)
  }

  def getSecurityProvider(schemaName: String)(implicit ctx: GeneratorContext): SecurityProvider = {
    ctx.settings.securityProviders.find(_.securitySchema == schemaName).getOrElse(SecurityProvider.default)
  }

  // NOTE Use only first security schema
  def getSecurityProvider(operation: Operation)(implicit ctx: GeneratorContext): SecurityProvider = {
    operation.security.headOption.map(s => getSecurityProvider(s.schemaName)).getOrElse(SecurityProvider.default)
  }

  case class MethodParam(valDef: ValDef, fullQualified: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree], defaultValue: Option[Tree], doc: DocElement)

  def getBodyParameters(path: Path, operation: Operation)
                       (implicit ctx: GeneratorContext): Seq[(String, MethodParam)] = {
    (path.parameters ++ operation.parameters).collect {
      case param: BodyParameter =>
        val paramName = decapitalize(param.name)
        val support = getTypeSupport(param.ref)
        val valDef = PARAM(paramName, support.tpe).empty
        val fullQualified = PARAM(paramName, support.fullQualified).empty
        val implicits = support.jsonReads ++ support.jsonWrites
        val doc = DocTag.Param(paramName, param.description.getOrElse(""))
        paramName -> MethodParam(valDef, fullQualified, support.definitions, implicits, None, doc)
    }.distinctBy(_._1).toIndexedSeq
  }

  def getMethodParameters(path: Path, operation: Operation, withHeaders: Boolean = true)
                         (implicit ctx: GeneratorContext): Seq[(String, MethodParam)] = {
    (path.parameters ++ operation.parameters).toIndexedSeq
      .filter {
        case _: HeaderParameter => withHeaders
        case _: PathParameter   => true
        case _: QueryParameter  => true
        case _: BodyParameter   => false
        case x =>
          println(s"Unmanaged parameter type for parameter '${x.name}' (operationId: ${operation.operationId}), please contact the developer to implement it XD")
          false
      }
      .sortBy { //the order must be verified...
        case _: HeaderParameter => 1
        case _: PathParameter   => 2
        case _: QueryParameter  => 3
        case _ => 4
      }
      .map { param =>
        getMethodParam(param)
      }
      .distinctBy(_._1)
  }

  def getMethodParam(param: Parameter)(implicit ctx: GeneratorContext): (String, MethodParam) = {
    val paramName = decapitalize(param.name)
    val defaultValue = getDefaultValue(param)
    val support = getTypeSupport(param.ref, DefinitionContext.default.copy(canBeOption = defaultValue.isEmpty))
    val valDef = defaultValue match {
      case Some(default) => PARAM(paramName, support.tpe) := support.constructor(default)
      case None => PARAM(paramName, support.tpe).empty
    }
    val fullQualified = PARAM(paramName, support.fullQualified).empty
    val doc = DocTag.Param(paramName, param.description.getOrElse(""))
    paramName -> MethodParam(valDef, fullQualified, support.definitions, getParamImplicits(param, support), defaultValue.map(support.constructor.apply), doc)
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

  def codeIsOk(code: Int): Boolean = code >= 200 && code < 300

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
      case StatusResponse(code) => getStatusByCode(code).getOrElse(code.toString)
    }).capitalize
  }

  def getResponseBodyType(response: Response)(implicit ctx: GeneratorContext): Option[TypeSupport] = {
    response.schema.map(
      body => getTypeSupport(body, DefinitionContext.default.copy(canBeInterface = true))(ctx.addCurrentPath("body"))
    )
  }

  def filterNonEmptyTree(trees: Seq[Tree]): Seq[Tree] = trees.filterNot(_ == EmptyTree)

  def distinctTreeByName(trees: Seq[Tree]): Seq[Tree] = {
    val UnknownName = "&Unknown&"
    var namesSet = Set.empty[String]
    for {
      definitionTree <- trees
      definitionName = definitionTree match {
        case ModuleDef(_, name, _) => name.name
        case valDef: ValOrDefDef => valDef.name.name
        case ProcDef(_, name, _, _, _) => name.name
        case TypeDef(_, name, _, _) => name.name
        case ClassDef(_, _, name, _, _, _) => name.name
        case _ => UnknownName
      }
      values <- if (definitionName == UnknownName) {
        Seq(definitionTree)
      } else if (!namesSet(definitionName)) {
        namesSet += definitionName
        Seq(definitionTree)
      } else {
        Nil
      }
    } yield values
  }

  implicit class IterableExtensionMethods[A, Repr](val xs: IterableLike[A, Repr]) extends AnyVal {

    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]): That = {
      val builder = cbf(xs.repr)
      val i = xs.iterator
      var set = Set[B]()
      while (i.hasNext) {
        val o = i.next
        val b = f(o)
        if (!set(b)) {
          set += b
          builder += o
        }
      }
      builder.result
    }

  }

}
