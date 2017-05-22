package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

trait InjectionProvider {

  def imports: Seq[Import]

  def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String

}

object InjectionProvider {

  case class Dependency(name: String, tpe: Type)

  class DefaultInConstructor extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val tree = treeToString(classDef)
      if (dependencies.nonEmpty) {
        val className = classDef.name.toString()
        val params = dependencies.map { case Dependency(name, tpe) =>
          s"$name: ${tpe.toString()}"
        }.mkString("(", ",", ")")
        tree.replaceFirst(className, className + params)
      } else {
        tree
      }
    }

  }

  class DefaultInMethods extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val values = dependencies.map { case Dependency(name, tpe) =>
        DEF(name, tpe).tree
      }
      val tree = classDef.copy(
        mods = classDef.mods | Flags.ABSTRACT,
        impl = classDef.impl.copy(
          body = values.toList ++ classDef.impl.body
        )
      )
      treeToString(tree)
    }

  }

}
