package play.boilerplate.generators

import play.boilerplate.parser.model._

class PlayJsonGeneratorParser extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods = schema.definitions.values
      .flatMap { model =>
        GeneratorUtils.getTypeSupport(model.ref)(ctx.setInModel(true)).defs
      }
      .groupBy(_.symbol.nameString)
      .mapValues(defs => filterNonEmptyTree(Seq(defs.head.jsonReads, defs.head.jsonWrites)))
      .values.flatten

    if (methods.nonEmpty) {

      val imports = BLOCK(
        IMPORT("play.api.libs.json", "_")
      ) inPackage ctx.settings.modelPackageName

      val packageObj = PACKAGEOBJECTDEF(ctx.settings.jsonObjectName) := BLOCK(methods)

      SourceCodeFile(
        packageName = ctx.settings.jsonPackageName,
        className = ctx.settings.jsonObjectName,
        header = treeToString(imports),
        impl = treeToString(packageObj)
      ) :: Nil

    } else {
      Nil
    }

  }

}
