package play.boilerplate.generators

import play.boilerplate.parser.model.Schema

trait CodeGenerator {
  def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile]
}
