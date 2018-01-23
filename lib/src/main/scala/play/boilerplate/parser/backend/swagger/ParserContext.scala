package play.boilerplate.parser.backend.swagger

case class ParserContext(refCanBeLazy: Boolean,
                         consumes: Iterable[String],
                         produces: Iterable[String])

object ParserContext {

  def initial = ParserContext(
    refCanBeLazy = true,
    consumes = Nil,
    produces = Nil
  )

}