package play.boilerplate.parser.backend

import play.boilerplate.parser.model.Schema

import scala.util.Try

trait ParserBackend {
  def parseSchema(fileName: String): Try[Schema]
}
