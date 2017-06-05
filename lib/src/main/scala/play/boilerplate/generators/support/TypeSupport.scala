package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorUtils._
import treehugger.forest._
import treehuggerDSL._

case class TypeSupport(tpe: Type, fullQualified: Type, defs: Seq[TypeSupportDefs]) {

  def definitions: Seq[Tree] = filterNonEmptyTree(defs.map(_.definition))

  def jsonReads : Seq[Tree] = filterNonEmptyTree(defs.map(_.jsonReads ))
  def jsonWrites: Seq[Tree] = filterNonEmptyTree(defs.map(_.jsonWrites))

  def queryBindable: Seq[Tree] = filterNonEmptyTree(defs.map(_.queryBindable))
  def pathBindable : Seq[Tree] = filterNonEmptyTree(defs.map(_.pathBindable ))

}

case class TypeSupportDefs(symbol: Symbol,
                           definition: Tree,
                           jsonReads: Tree,
                           jsonWrites: Tree,
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
