package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

final class ScaldiInjectionProvider extends InjectionProvider {

  override val imports: Seq[Import] = {
    Seq(IMPORT("scaldi", "Injectable", "Injector"))
  }

  override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
    val injects = dependencies.flatMap { case Dependency(name, tpe) =>
      val valName = "_" + name
      Seq(
        VAL(valName, tpe).withFlags(Flags.LAZY, Flags.PRIVATE) := REF("inject").APPLYTYPE(tpe),
        DEF(name, tpe) := REF(valName)
      )
    }
    val injector = PARAM("inj", TYPE_REF("Injector")).withFlags(Flags.IMPLICIT).tree
    val classTree = classDef.copy(
      impl = classDef.impl.copy(
        parents = classDef.impl.parents :+ REF("Injectable"),
        body = injects.toList ++ classDef.impl.body
      )
    )
    val tree = treeToString(classTree)
    tree
      .replaceFirst(s"\\)\\s*\\{", ")(" + treeToString(injector) + ") {")
      .replaceFirst(s"\\)\\s*extends", ")(" + treeToString(injector) + ") extends")
  }

}