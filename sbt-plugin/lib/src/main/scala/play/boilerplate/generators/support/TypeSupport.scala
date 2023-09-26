package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorUtils._
import treehugger.forest._
import treehuggerDSL._

case class TypeSupport(tpe: Type,
                       fullQualified: Type,
                       defs: Seq[TypeSupportDefs],
                       constructor: Tree => Tree = identity) {

  def definitions: Seq[Tree] = filterNonEmptyTree(defs.map(_.definition))

  def jsonReads : Seq[Tree] = filterNonEmptyTree(defs.map(_.jsonReads ))
  def jsonWrites: Seq[Tree] = filterNonEmptyTree(defs.map(_.jsonWrites))

  def queryBindable: Seq[Tree] = filterNonEmptyTree(defs.map(_.queryBindable))
  def pathBindable : Seq[Tree] = filterNonEmptyTree(defs.map(_.pathBindable ))

  def queryParameter: Seq[Tree] = filterNonEmptyTree(defs.map(_.queryParameter))
  def pathParameter : Seq[Tree] = filterNonEmptyTree(defs.map(_.pathParameter))

}

object TypeSupport {

  def generateJsonObject(symbol: Symbol, jsonReads: Seq[Tree], jsonWrites: Seq[Tree]): Tree = {
    val jsonMethods = filterNonEmptyTree(jsonReads ++ jsonWrites)
    if (jsonMethods.nonEmpty) {
      OBJECTDEF(symbol.nameString) := BLOCK(jsonMethods)
    } else {
      EmptyTree
    }
  }

}

case class TypeSupportDefs(symbol: Symbol,
                           definition: Tree,
                           jsonReads: Tree,
                           jsonWrites: Tree,
                           queryBindable: Tree,
                           pathBindable: Tree,
                           queryParameter: Tree,
                           pathParameter: Tree)

object TypeSupportDefs {

  def uniqueWithSameOrder(defs: Seq[TypeSupportDefs]): Seq[TypeSupportDefs] =
    defs.zipWithIndex.groupBy(_._1.symbol.nameString).mapValues(_.head).values.toSeq.sortBy(_._2).map(_._1)

}
