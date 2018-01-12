package play.boilerplate.generators.injection

import InjectionProvider._
import treehugger.forest._
import treehuggerDSL._

trait InjectionProvider {

  def imports: Seq[Import]

  def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String

}

object InjectionProvider {

  case class Dependency(name: String,
                        tpe: Type,
                        defaultValue: Option[Tree] = None,
                        isOverride: Boolean = false,
                        isImplicit: Boolean = false
                       ) {
    val flags: Seq[Long] = {
      Seq(Flags.OVERRIDE.toLong).filter(_ => isOverride) ++
      Seq(Flags.IMPLICIT.toLong).filter(_ => isImplicit)
    }
  }

  class DefaultInConstructor extends InjectionProvider {

    override def imports: Seq[Import] = Nil

    override def classDefModifier(classDef: ClassDef, dependencies: Seq[Dependency]): String = {
      val tree = treeToString(classDef)
      if (dependencies.nonEmpty) {
        val className = classDef.name.toString()
        val (params, implicitParams) = dependencies.foldLeft((List.empty[String], List.empty[String])) {
          case ((p, i), Dependency(name, tpe, defaultValue, isOverride, isImplicit)) =>
            val valDef = s"$name: ${tpe.toString()}"
            val valDefWithFlags = if (isOverride) s"override val " + valDef else valDef
            val paramDef = defaultValue.fold(valDefWithFlags)(v => valDefWithFlags + " = " + treeToString(v))
            if (isImplicit) (p, i :+ paramDef) else (p :+ paramDef, i)
        }
        val paramsList = params.mkString("(", ", ", ")") + implicitParams.mkString("(implicit ", ", ", ")")
        tree.replaceFirst(className, className + paramsList)
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
        case dep @ Dependency(name, tpe, defaultValue, _, _) =>
          val methodDef = DEF(name, tpe)
          val methodWithFlags = methodDef.withFlags(dep.flags: _ *)
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
