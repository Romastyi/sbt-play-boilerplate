package play.boilerplate.generators

import play.boilerplate.parser.model._

class JsonCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val methods: Iterable[Tree] = schema.definitions.values
      .flatMap { model =>
        GeneratorUtils.getTypeSupport(model.ref)(ctx.setCurrentModel(Some(model))).defs
      }
      .groupBy(_.symbol.nameString)
      .mapValues(defs => filterNonEmptyTree(Seq(defs.head.jsonReads, defs.head.jsonWrites)))
      .values.flatten

    val imports = BLOCK(
      IMPORT(REF(ctx.settings.modelPackageName), "_"),
      IMPORT(REF("play.api.data.validation"), "_"),
      IMPORT(REF("play.api.libs.json"), "_"),
      IMPORT(REF("play.api.libs.functional.syntax"), "_")
    ) inPackage ctx.settings.jsonPackageName

    val objDef = OBJECTDEF(ctx.settings.jsonObjectName) := {
      if (methods.nonEmpty) BLOCK(methods) else EmptyTree
    }

    SourceCodeFile(
      packageName = ctx.settings.jsonPackageName,
      className = ctx.settings.jsonObjectName,
      header = treeToString(imports),
      impl = treeToString(objDef)
    ) :: Nil

  }

}
