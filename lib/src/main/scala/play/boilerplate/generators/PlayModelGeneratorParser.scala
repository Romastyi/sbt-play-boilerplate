package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.parser.model._

class PlayModelGeneratorParser {

  import treehugger.forest._
  import treehuggerDSL._

  def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[SyntaxString] = {

    val init = EmptyTree inPackage ctx.modelPackageName

    for {
      (name, model) <- schema.definitions
    } yield SyntaxString(
      name = name,
      pre = treeToString(init),
      impl = treeToString(generateClass(model)(ctx.setInModel(true)): _ *)
    )

  }

  private def generateClass(model: Model)(implicit ctx: GeneratorContext): Seq[Tree] = {
    GeneratorUtils.getTypeSupport(model.ref).definitions
  }

}
