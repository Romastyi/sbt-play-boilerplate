package play.boilerplate.generators

import play.boilerplate.generators.support.TypeSupportDefs
import play.boilerplate.parser.model._

class JsonCodeGenerator extends CodeGenerator {

  import GeneratorUtils._
  import treehugger.forest._
  import treehuggerDSL._

  override def generate(schema: Schema)(implicit ctx: GeneratorContext): Iterable[CodeFile] = {

    val definitions = TypeSupportDefs.uniqueWithSameOrder(
      schema.definitions.values
        .flatMap { model =>
          GeneratorUtils.getTypeSupport(model.ref)(ctx.setCurrentModel(Some(model))).defs
        }
        .toSeq
    )
    val methods: Iterable[Tree] = definitions.flatMap {
      definition => filterNonEmptyTree(Seq(definition.jsonReads, definition.jsonWrites))
    }

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
