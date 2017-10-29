package play.boilerplate.generators

import play.boilerplate.parser.model._

class ModelCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val init = EmptyTree inPackage ctx.settings.modelPackageName

    for {
      (name, model) <- schema.definitions
      impl = filterNonEmptyTree(generateClass(model))
      if impl.nonEmpty
    } yield SourceCodeFile(
      packageName = ctx.settings.modelPackageName,
      className = name,
      header = treeToString(init),
      impl = treeToString(impl: _ *)
    )

  }

  private def generateClass(model: Model)(implicit ctx: GeneratorContext): Seq[Tree] = {
    GeneratorUtils.getTypeSupportRef(model).definitions
  }

}
