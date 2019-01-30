package play.boilerplate.parser.model

object HttpStatus {

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
    500 -> "InternalServerError",
    501 -> "NotImplemented",
    502 -> "BadGateway",
    503 -> "ServiceUnavailable",
    504 -> "GatewayTimeout",
    505 -> "HttpVersionNotSupported",
    507 -> "InsufficientStorage"
  )

  def codeIsOk(code: Int): Boolean = code >= 200 && code < 300

  def findStatusByCode(code: Int): Option[String] = statusByCode.get(code)

  def getStatusByCode(code: Int): String = findStatusByCode(code).getOrElse(code.toString)

}
