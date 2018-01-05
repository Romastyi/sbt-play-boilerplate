package play.boilerplate.api.common

import play.api.mvc.QueryStringBindable

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

object BindersMacroImpl {

  def queryStringStrictImpl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[QueryStringBindable[A]] = {
    macroImpl[A](c, strict = true)
  }

  def queryStringImpl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[QueryStringBindable[A]] = {
    macroImpl[A](c, strict = false)
  }

  private def macroImpl[A: c.WeakTypeTag](c: blackbox.Context, strict: Boolean): c.Expr[QueryStringBindable[A]] = {

    import c.universe._

    val tpe = weakTypeOf[A]
    val companioned = tpe.typeSymbol
    val companionObject = companioned.companion
    val companionType = companionObject.typeSignature

    val queryBindable = weakTypeOf[QueryStringBindable[A]]

    val applies = companionType.decl(TermName("apply")) match {
      case NoSymbol =>
        c.abort(c.enclosingPosition, "No apply function found")
      case s =>
        s.asTerm.alternatives.filter(
          _.asMethod.paramLists.size == 1
        )
    }

    val applyMethod = applies.collectFirst {
      case method: MethodSymbol if method.paramLists.head.nonEmpty => method
    }.getOrElse {
      c.abort(c.enclosingPosition, "No apply function found with parameters")
    }

    val params = applyMethod.paramLists.head

    final case class Param(paramName: Name, paramType: Type, termName: TermName, binderImplicit: Tree, defaultValue: Option[Tree])

    val applyParamWithImplicits = params.zipWithIndex.map { case (param, idx) =>
      val ptype = param.typeSignature
      val needImplicitType = appliedType(queryBindable.typeConstructor, ptype :: Nil)
      val needImplicit = c.inferImplicitValue(needImplicitType)
      if (needImplicit == EmptyTree) {
        c.abort(c.enclosingPosition, s"No QueryString binder found for type $ptype.")
      }
      val pterm = param.asTerm
      val defaultValue = if (pterm.isParamWithDefault) {
        val getterName = TermName("apply$default$" + (idx + 1))
        Some(q"$companionObject.$getterName")
      } else {
        None
      }
      Param(param.name, ptype, pterm.name, needImplicit, defaultValue)
    }

    final case class Parts(binder: Tree, value: Tree, valueFq: Tree, interimFq: Tree, unbind: Tree)

    val parts = applyParamWithImplicits.zipWithIndex.map {
      case (Param(name, ptype, tname, impl, default), idx) =>
        val pname  = "." + name.decodedName.toString
        val binder = TermName(s"binder${idx + 1}")
        val value  = TermName(s"value${idx + 1}")
        val interim = TermName(s"opt${idx + 1}")
        val isOption = ptype.baseType(typeOf[Option[_]].typeSymbol) != NoType
        val (interimFq, valueFq, valueP) = default match {
          case Some(v) if isOption =>
            val defaultValue = q"""Right[String, $ptype](None)"""
            (
              fq"""$interim <- $binder.bind(key + $pname, params).orElse(Some($defaultValue))""",
              if (strict) {
                fq"""$value <- $binder.bind(key + $pname, params).getOrElse($defaultValue).right"""
              } else {
                fq"""$value <- $interim.right"""
              },
              q"""$value.orElse($v)"""
            )
          case Some(v) =>
            val defaultValue = q"""Right[String, $ptype]($v)"""
            (
              fq"""$interim <- $binder.bind(key + $pname, params).orElse(Some($defaultValue))""",
              if (strict) {
                fq"""$value <- $binder.bind(key + $pname, params).getOrElse($defaultValue).right"""
              } else {
                fq"""$value <- $interim.right"""
              },
              q"""$value"""
            )
          case None =>
            val error = q"""Left[String, $ptype]("Missing parameter: " + key + $pname)"""
            (
              fq"""$interim <- $binder.bind(key + $pname, params)""",
              if (strict) {
                fq"""$value <- $binder.bind(key + $pname, params).getOrElse($error).right"""
              } else {
                fq"""$value <- $interim.right"""
              },
              q"""$value"""
            )
        }
        Parts(
          q"""private val $binder = $impl""",
          valueP,
          valueFq,
          interimFq,
          q"""$binder.unbind(key + $pname, value.$tname)"""
        )
    }

    val unbind = parts.map(_.unbind).reduce[Tree]((acc, part) => q"""$acc + "&" + $part""")

    val resultTree = if (strict) {
      q"""
      new $queryBindable {
        ..${parts.map(_.binder)}
        override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, $tpe]] = {
          Some(
            for (..${parts.map(_.valueFq)}) yield $companionObject.apply(..${parts.map(_.value)})
          )
        }
        override def unbind(key: String, value: $tpe): String = {
          $unbind
        }
      }"""
    } else {
      q"""
      new $queryBindable {
        ..${parts.map(_.binder)}
        override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, $tpe]] = {
          for (..${parts.map(_.interimFq)}) yield {
            for (..${parts.map(_.valueFq)}) yield $companionObject.apply(..${parts.map(_.value)})
          }
        }
        override def unbind(key: String, value: $tpe): String = {
          $unbind
        }
      }"""
    }

    /*println(resultTree)*/

    c.Expr[QueryStringBindable[A]](resultTree)
  }
}
