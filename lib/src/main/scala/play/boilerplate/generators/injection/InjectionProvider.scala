package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

trait InjectionProvider {

  def imports: Seq[Import]

  def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String

}

object InjectionProvider {

  case class Dependency(name: String, tpe: Type, defaultValue: Option[Tree] = None, isOverride: Boolean = false)

  class DefaultInConstructor extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val tree = treeToString(classDef)
      if (dependencies.nonEmpty) {
        val className = classDef.name.toString()
        val params = dependencies.map {
          case Dependency(name, tpe, defaultValue, isOverride) =>
            val valDef = s"$name: ${tpe.toString()}"
            val valDefWithFlags = if (isOverride) s"override val " + valDef else valDef
            defaultValue.fold(valDefWithFlags)(v => valDefWithFlags + " = " + treeToString(v))
        }.mkString("(implicit ", ", ", ")")
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
        case Dependency(name, tpe, defaultValue, isOverride) =>
          val methodDef = DEF(name, tpe)
          val methodWithFlags = if (isOverride) methodDef.withFlags(Flags.OVERRIDE) else methodDef
          defaultValue.fold(methodWithFlags.empty)(v => methodDef := v)
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
