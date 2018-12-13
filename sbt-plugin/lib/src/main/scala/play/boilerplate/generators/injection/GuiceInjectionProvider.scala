package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

object GuiceInjectionProvider extends DefaultInConstructor {

  override val imports: Seq[Import] = {
    Seq(IMPORT(REF("javax.inject"), "_"))
  }

  override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
    val tree = super.classDefModifier(classDef, dependencies)
    if (dependencies.nonEmpty) {
      val className = classDef.name.toString()
      tree.replaceFirst(s"$className\\s*\\(", className + " @Inject() (")
    } else {
      tree
    }
  }

}