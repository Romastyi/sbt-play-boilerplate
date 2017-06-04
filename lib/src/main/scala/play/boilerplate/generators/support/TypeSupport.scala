package play.boilerplate.generators.support

import treehugger.forest._

case class TypeSupport(tpe: Type, fullQualified: Type, defs: Seq[TypeSupportDefs])
case class TypeSupportDefs(definition: Tree, jsonReads: Tree, jsonWrites: Tree, queryBindable: Tree, pathBindable: Tree)
