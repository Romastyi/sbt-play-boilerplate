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
  final def PAIR(key: Tree, value: Tree): Tree = key INFIX ("->", value)
  final def RANDOM_UUID: Tree = REF("java.util.UUID") DOT "randomUUID()"
  final def RANDOM_UUID_STRING: Tree = REF("java.util.UUID") DOT "randomUUID()" DOT "toString"

  final val F_TYPEVAR = TYPEVAR("F").withTypeParams(TYPEVAR("_"))
  final def F_OF_TYPE(tpe: Type): Type = TYPE_REF("F") TYPE_OF tpe

  final val MIME_TYPE_JSON = "application/json"
  final val MIME_TYPE_TEXT = "text/plain"
  final val MIME_TYPE_MULTIPART_FORMDATA = "multipart/form-data"
  final val MIME_TYPE_FORM_URLENCODED = "application/x-www-form-urlencoded"

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

  def supportedMimeTypes(implicit ctx: GeneratorContext): Set[String] = {
    Set(MIME_TYPE_JSON, MIME_TYPE_FORM_URLENCODED, MIME_TYPE_MULTIPART_FORMDATA) ++ ctx.settings.supportedMimeTypes.keySet
  }

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

  def getFullParametersList(path: Path, operation: Operation): Seq[Parameter] = {
    (path.parameters ++ operation.parameters).toIndexedSeq
  }

  sealed trait BodyContentType
  case object NoContent extends BodyContentType
  case object SimpleContent extends BodyContentType
  case object MultipartFormData extends BodyContentType
  case object FormUrlencoded extends BodyContentType

  def getBodyContentType(schema: Schema, path: Path, operation: Operation): BodyContentType = {
    val consumes = (schema.consumes ++ operation.consumes).toSet
    val fullParamList = getFullParametersList(path, operation)
    val formDataParameters = fullParamList.collect {
      case formData: FormParameter => formData
    }
    if (formDataParameters.nonEmpty) {
      val hasFiles = formDataParameters.exists { _.baseDef match {
        case _: FileDefinition => true
        case _ => false
      }}
      if (consumes(MIME_TYPE_FORM_URLENCODED) && !hasFiles) FormUrlencoded else MultipartFormData
    } else {
      val bodyParameters = fullParamList.collect {
        case param: BodyParameter => param
      }
      if (bodyParameters.nonEmpty) SimpleContent else NoContent
    }
  }

  case class MethodParam(valDef: ValDef, fullQualified: ValDef, additionalDef: Seq[Tree], implicits: Seq[Tree], defaultValue: Option[Tree], isOptional: Boolean, doc: DocElement)

  def getBodyParameters(path: Path, operation: Operation)
                       (implicit ctx: GeneratorContext): Seq[(String, MethodParam)] = {
    getFullParametersList(path, operation).collect {
      case param: BodyParameter =>
        val paramName = getParameterIdentifier(param)
        val support = getTypeSupport(param.ref)
        val valDef = PARAM(paramName, support.tpe).empty
        val fullQualified = PARAM(paramName, support.fullQualified).empty
        val implicits = support.jsonReads ++ support.jsonWrites
        val doc = DocTag.Param(paramName, param.description.getOrElse(""))
        paramName -> MethodParam(valDef, fullQualified, support.definitions, implicits, None, isOptional = false, doc)
    }.distinctBy(_._1).toIndexedSeq
  }

  def getMethodParameters(path: Path, operation: Operation, withHeaders: Boolean = true, withFormData: Boolean = true)
                         (implicit ctx: GeneratorContext): Seq[(String, MethodParam)] = {

    val operationParams = getFullParametersList(path, operation)
      .filter {
        case h: HeaderParameter => withHeaders && !isTraceIdHeaderParameter(h)
        case _: FormParameter   => withFormData
        case _: PathParameter   => true
        case _: QueryParameter  => true
        case _: BodyParameter   => false
        case x =>
          println(s"Unmanaged parameter type for parameter '${x.name}' (operationId: ${operation.operationId}), please contact the developer to implement it XD")
          false
      }
      .sortBy { //the order must be verified...
        case _: PathParameter   => 1
        case _: QueryParameter  => 2
        case _: FormParameter   => 3
        case _: HeaderParameter => 4
        case _ => 5
      }
      .map { param =>
        getMethodParam(param)
      }
      .distinctBy(_._1)

    val traceIdParam = if (ctx.settings.useTraceId && withHeaders) {
      Seq(traceIdValName -> MethodParam(
        valDef = PARAM(traceIdValName, StringClass).empty,
        fullQualified = PARAM(traceIdValName, StringClass).empty,
        additionalDef = Nil,
        implicits = Nil,
        defaultValue = None,
        isOptional = false,
        doc = DocTag.Param(traceIdValName, "Request Trace ID,")
      ))
    } else {
      Nil
    }

    operationParams ++ traceIdParam

  }

  def getMethodParam(param: Parameter)(implicit ctx: GeneratorContext): (String, MethodParam) = {
    val paramName = getParameterIdentifier(param)
    val defaultValue = getDefaultValue(param)
    val support = getTypeSupport(param.ref, DefinitionContext.default.copy(canBeOption = defaultValue.isEmpty))
    val valDef = defaultValue match {
      case Some(default) => PARAM(paramName, support.tpe) := support.constructor(default)
      case None => PARAM(paramName, support.tpe).empty
    }
    val fullQualified = PARAM(paramName, support.fullQualified).empty
    val doc = DocTag.Param(paramName, param.description.getOrElse(""))
    paramName -> MethodParam(valDef, fullQualified, support.definitions, getParamImplicits(param, support), defaultValue.map(support.constructor.apply), isOptional(param), doc)
  }

  def getParamImplicits(param: Parameter, support: TypeSupport)(implicit ctx: GeneratorContext): Seq[Tree] = {
    param match {
      case _: PathParameter if ctx.inClient =>
        support.pathParameter
      case _: PathParameter =>
        support.pathBindable
      case _: QueryParameter if ctx.inClient =>
        support.queryParameter
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

  def isOptional(parameter: Parameter): Boolean = isOptional(parameter.ref)
  def isOptional(definition: Definition): Boolean = definition match {
    case _: OptionDefinition => true
    case _ => false
  }

  // TraceID

  final val traceIdValName = "traceId"
  final val traceIdValRef = REF(traceIdValName)

  def findHeaderParameter(path: Path, operation: Operation, headerName: String): Option[HeaderParameter] = {
    getFullParametersList(path, operation).find {
      case h: HeaderParameter if h.name == headerName => true
      case _ => false
    }.collect {
      case h: HeaderParameter => h
    }
  }

  def isTraceIdHeaderParameter(param: HeaderParameter)(implicit ctx: GeneratorContext): Boolean = {
    ctx.settings.useTraceId && ctx.settings.traceIdHeader == Some(param.name)
  }

  /*
   * final case class UnexpectedResult(body: String = "", code: Int = 200) extends ...
   */
  final val UnexpectedResult = TypeName("UnexpectedResult")

  def getOperationResponseTraitName(operationId: String): String = {
    operationId.capitalize + "Response"
  }

  def getResponseClassName(operationId: String, responseCode: ResponseCode): String = {
    operationId.capitalize + (responseCode match {
      case DefaultResponse => "Default"
      case StatusResponse(code) => HttpStatus.getStatusByCode(code)
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
