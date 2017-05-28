package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString

trait PrintSyntaxString {

  def printSyntaxString(ss: Iterable[SyntaxString]): Unit = {

    for (SyntaxString(name, pre, impl) <- ss) yield {
      println(s"--- $name ---")
      println(pre)
      println(impl)
    }

  }

}
