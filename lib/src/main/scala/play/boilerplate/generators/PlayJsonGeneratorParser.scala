package play.boilerplate.generators

import play.boilerplate.parser.model._

class PlayJsonGeneratorParser extends CodeGenerator {

  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods = schema.definitions.values.flatMap { model =>
      val support = GeneratorUtils.getTypeSupport(model.ref)(ctx.setInModel(true))
      support.jsonReads ++ support.jsonWrites
    }

    if (methods.nonEmpty) {

      val imports = BLOCK(
        IMPORT(ctx.settings.modelPackageName, "_"),
        IMPORT("play.api.libs.json", "_")
      ) inPackage ctx.settings.jsonPackageName

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
