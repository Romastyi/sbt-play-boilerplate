package play.boilerplate.parser.model

object HttpMethod extends Enumeration {
  val Get = Value("GET")
  val Put = Value("PUT")
  val Post = Value("POST")
  val Head = Value("HEAD")
  val Delete = Value("DELETE")
  val Patch = Value("PATCH")
  val Options = Value("OPTIONS")
}
