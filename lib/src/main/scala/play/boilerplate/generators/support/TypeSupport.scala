package play.boilerplate.generators.support

import treehugger.forest._
import treehuggerDSL._

case class TypeSupport(tpe: Type, fullQualified: Type, defs: Seq[TypeSupportDefs])
case class TypeSupportDefs(symbol: Symbol,
                           definition: Tree,
                           jsonReads: Tree,
                           jsonWrites: Tree,
                           jsonObject: Tree,
                           queryBindable: Tree,
                           pathBindable: Tree)

object TypeSupport {

  def generateJsonObject(symbol: Symbol, jsonReads: Tree, jsonWrites: Tree): Tree = {
    val jsonMethods = Seq(jsonReads, jsonWrites).filterNot(_ == EmptyTree)
    if (jsonMethods.nonEmpty) {
      OBJECTDEF(symbol.nameString) := BLOCK(jsonMethods)
    } else {
      EmptyTree
    }
  }

}
