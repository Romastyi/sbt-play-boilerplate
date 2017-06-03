package play.boilerplate.parser.backend.swagger

case class ParserContext(refCanBeLazy: Boolean)

object ParserContext {

  def initial = ParserContext(
    refCanBeLazy = true
  )

}