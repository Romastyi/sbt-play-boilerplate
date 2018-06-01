package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

object ScaldiInjectionProvider extends InjectionProvider {

  override val imports: Seq[Import] = {
    Seq(IMPORT(REF("scaldi"), "Injectable", "Injector"))
  }

  override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
    val injects = dependencies.flatMap {
      case dep @ Dependency(name, tpe, defaultValue, _, _) =>
        val valName = "_" + name
        val methodDef = DEF(name, tpe).withFlags(dep.flags: _ *)
        val injection: Tree = REF("inject") APPLYTYPE tpe
        val injectionWithDefault = defaultValue.fold(injection)(
          v => injection APPLY REF("by default " + treeToString(v))
        )
        Seq(
          VAL(valName, tpe).withFlags(Flags.LAZY, Flags.PRIVATE) := injectionWithDefault,
          methodDef := REF(valName)
        )
    }
    val injector = PARAM("inj", TYPE_REF("Injector")).withFlags(Flags.IMPLICIT).tree
    val className = classDef.name.toString()
    val classTree = classDef.copy(
      impl = classDef.impl.copy(
        parents = classDef.impl.parents :+ REF("Injectable"),
        body = injects.toList ++ classDef.impl.body
      )
    )
    val tree = treeToString(classTree)
    tree
      //.replaceFirst(s"\\)\\s*\\{", ")(" + treeToString(injector) + ") {")
      //.replaceFirst(s"\\)\\s*extends", ")(" + treeToString(injector) + ") extends")
      .replaceFirst(s"$className\\s*\\{", className + "(" + treeToString(injector) + ") {")
      .replaceFirst(s"$className\\s*extends", className + "(" + treeToString(injector) + ") extends")
  }

}