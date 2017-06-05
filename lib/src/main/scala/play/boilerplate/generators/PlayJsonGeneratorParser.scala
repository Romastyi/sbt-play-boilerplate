package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.parser.model._

class PlayJsonGeneratorParser(schema: Schema) {

  import treehugger.forest._
  import treehuggerDSL._

  def generate(implicit ctx: GeneratorContext): Iterable[SyntaxString] = {

    val methods = schema.definitions.values.flatMap { model =>
      val support = GeneratorUtils.getTypeSupport(model.ref)(ctx.setInModel(true))
      support.jsonReads ++ support.jsonWrites
    }

    if (methods.nonEmpty) {

      val imports = BLOCK {
        Seq(IMPORT("play.api.libs.json", "_"))
      } inPackage ctx.jsonPackageName

      val packageObj = PACKAGEOBJECTDEF(ctx.jsonObjectName) := BLOCK(methods)

      SyntaxString(ctx.jsonObjectName, treeToString(imports), treeToString(packageObj)) :: Nil

    } else {
      Nil
    }

  }

}
