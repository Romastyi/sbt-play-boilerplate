package play.boilerplate.generators

trait PrintSyntaxString {

  def printCodeFile(codeFile: Iterable[CodeFile]): Unit = {
    codeFile.foreach { source =>
      println(s"--- ${source.fileName} ---")
      println(source.source)
    }
  }

  def printRoutes(routes: Seq[String]): Unit = {
    println("--- routes ---")
    routes.foreach(println)
  }

}
