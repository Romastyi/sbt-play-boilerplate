package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

final class ScaldiInjectionProvider extends InjectionProvider {

  override val imports: Seq[Import] = {
    Seq(IMPORT("scaldi", "Injectable", "Injector"))
  }

  override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
    val injects = dependencies.map { case Dependency(name, tpe) =>
      VAL(name, tpe).withFlags(Flags.LAZY) := REF("inject").APPLYTYPE(tpe)
    }
    val injector = PARAM("inj", TYPE_REF("Injector")).withFlags(Flags.IMPLICIT).tree
    val tree = classDef.copy(
      vparams = classDef.vparams :+ injector,
      impl = classDef.impl.copy(
        parents = classDef.impl.parents :+ REF("Injectable"),
        body = injects.toList ++ classDef.impl.body
      )
    )
    treeToString(tree)
  }

}