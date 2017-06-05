package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString

trait PrintSyntaxString {

  def printCodeFile(codeFile: Iterable[CodeFile]): Unit = {

    codeFile.foreach { source =>
      println(s"--- ${source.fileName} ---")
      println(source.source)
    }

  }

  def printSyntaxString(ss: Iterable[SyntaxString]): Unit = {

    for (SyntaxString(name, pre, impl) <- ss) yield {
      println(s"--- $name ---")
      println(pre)
      println(impl)
    }

  }

  def printRoutes(routes: Seq[String]): Unit = {
    println("--- routes ---")
    routes.foreach(println)
  }

}
