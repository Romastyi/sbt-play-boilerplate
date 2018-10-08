package play.boilerplate.generators

import play.boilerplate.parser.model._

class ModelCodeGenerator(inOneFile: Boolean) extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val init = EmptyTree inPackage ctx.settings.modelPackageName

    val sources = for {
      (name, model) <- schema.definitions
      impl = filterNonEmptyTree(generateClass(model)(ctx.setModelsInOneFile(inOneFile)))
      if impl.nonEmpty
    } yield SourceCodeFile(
      packageName = ctx.settings.modelPackageName,
      className = name,
      header = treeToString(init),
      impl = treeToString(impl: _ *)
    )

    if (sources.nonEmpty && inOneFile) {
      SourceCodeFile(
        packageName = ctx.settings.modelPackageName,
        className = ctx.settings.modelSingleFileName,
        header = treeToString(init),
        impl = sources.map(_.impl).mkString("\n\n")
      ) :: Nil
    } else {
      sources
    }

  }

  private def generateClass(model: Model)(implicit ctx: GeneratorContext): Seq[Tree] = {
    GeneratorUtils.getTypeSupportRef(model)(ctx.setCurrentModel(Some(model))).definitions
  }

}
