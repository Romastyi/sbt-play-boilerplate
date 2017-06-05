package play.boilerplate.generators

import eu.unicredit.swagger.generators.SyntaxString

trait PrintSyntaxString {

  def printCodeFile(codeFile: Iterable[CodeFile]): Unit = {

    codeFile.foreach {
      case source @ SourceCodeFile(_, header, impl) =>
        println(s"--- ${source.fileName} ---")
        println(header)
        println(impl)
      case ResourceFile(fileName, source) =>
        println(s"--- $fileName ---")
        println(source)
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
