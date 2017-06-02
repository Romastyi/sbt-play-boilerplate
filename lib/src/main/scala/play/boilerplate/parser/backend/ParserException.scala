package play.boilerplate.parser.backend

case class ParserException(msg: String, cause: Throwable) extends Exception(msg, cause)
