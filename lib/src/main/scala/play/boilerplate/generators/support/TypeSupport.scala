package play.boilerplate.generators.support

import treehugger.forest._
import treehuggerDSL._

case class TypeSupport(tpe: Type, fullQualified: Type, defs: Seq[TypeSupportDefs]) {

  def jsonReads : Seq[Tree] = defs.map(_.jsonReads) .filterNot(_ == EmptyTree)
  def jsonWrites: Seq[Tree] = defs.map(_.jsonWrites).filterNot(_ == EmptyTree)
  def jsonObject: Seq[Tree] = defs.map(_.jsonObject).filterNot(_ == EmptyTree)

  def queryBindable: Seq[Tree] = defs.map(_.queryBindable).filterNot(_ == EmptyTree)
  def pathBindable : Seq[Tree] = defs.map(_.pathBindable ).filterNot(_ == EmptyTree)

}

case class TypeSupportDefs(symbol: Symbol,
                           definition: Tree,
                           jsonReads: Tree,
                           jsonWrites: Tree,
                           jsonObject: Tree,
                           queryBindable: Tree,
                           pathBindable: Tree)

object TypeSupport {

  def generateJsonObject(symbol: Symbol, jsonReads: Seq[Tree], jsonWrites: Seq[Tree]): Tree = {
    val jsonMethods = (jsonReads ++ jsonWrites).filterNot(_ == EmptyTree)
    if (jsonMethods.nonEmpty) {
      OBJECTDEF(symbol.nameString) := BLOCK(jsonMethods)
    } else {
      EmptyTree
    }
  }

}
