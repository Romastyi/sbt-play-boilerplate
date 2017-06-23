package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

trait InjectionProvider {

  def imports: Seq[Import]

  def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String

}

object InjectionProvider {

  case class Dependency(name: String, tpe: Type, isOverride: Boolean = false)

  class DefaultInConstructor extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val tree = treeToString(classDef)
      if (dependencies.nonEmpty) {
        val className = classDef.name.toString()
        val params = dependencies.map {
          case Dependency(name, tpe, true) =>
            s"override val $name: ${tpe.toString()}"
          case Dependency(name, tpe, false) =>
            s"$name: ${tpe.toString()}"
        }.mkString("(", ", ", ")")
        tree.replaceFirst(className, className + params)
      } else {
        tree
      }
    }

  }

  def defaultInConstructor: InjectionProvider = new DefaultInConstructor()

  class DefaultInMethods extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val values = dependencies.map {
        case Dependency(name, tpe, true) =>
          DEF(name, tpe).withFlags(Flags.OVERRIDE).tree
        case Dependency(name, tpe, false) =>
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

  def defaultInMethods: InjectionProvider = new DefaultInMethods()

}
