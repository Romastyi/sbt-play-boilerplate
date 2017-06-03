package play.boilerplate.parser.backend

import play.boilerplate.parser.model.Schema

trait ParserBackend {

  def parseSchema(fileName: String): Either[Throwable, Schema]

}
