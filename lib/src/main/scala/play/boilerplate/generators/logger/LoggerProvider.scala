package play.boilerplate.generators.logger

import treehugger.forest._
import treehuggerDSL._

trait LoggerProvider {

  def loggerDefs: Seq[Tree]

  def imports: Seq[Import]

  def parents: Seq[Type]

  def selfTypes: Seq[Type]

  def trace(message: Tree): Tree

  def debug(message: Tree): Tree

  def info(message: Tree): Tree

  def warning(message: Tree): Tree

  def warning(message: Tree, cause: Ident): Tree

  def error(message: Tree, cause: Ident): Tree

  def fatal(message: Tree, cause: Ident): Tree

}

object LoggerProvider {

  def defaultPlayLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Seq(
      VAL("logger") := REF("Logger") APPLY (THIS DOT "getClass" DOT "getName")
    )

    override def imports: Seq[Import] = IMPORT("play.api", "Logger") :: Nil

    override def parents: Seq[Type] = Nil

    override def selfTypes: Seq[Type] = Nil

    override def trace(message: Tree): Tree = REF("logger") DOT "trace" APPLY message

    override def debug(message: Tree): Tree = REF("logger") DOT "debug" APPLY message

    override def info(message: Tree): Tree = REF("logger") DOT "info" APPLY message

    override def warning(message: Tree): Tree = REF("logger") DOT "warn" APPLY message

    override def warning(message: Tree, cause: Ident): Tree = REF("logger") DOT "warn" APPLY (message, cause)

    override def error(message: Tree, cause: Ident): Tree = REF("logger") DOT "error" APPLY (message, cause)

    override def fatal(message: Tree, cause: Ident): Tree = error(message, cause)

  }

  def withoutLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Nil

    override def imports: Seq[Import] = Nil

    override def parents: Seq[Type] = Nil

    override def selfTypes: Seq[Type] = Nil

    override def trace(message: Tree): Tree = EmptyTree

    override def debug(message: Tree): Tree = EmptyTree

    override def info(message: Tree): Tree = EmptyTree

    override def warning(message: Tree): Tree = EmptyTree

    override def warning(message: Tree, cause: Ident): Tree = EmptyTree

    override def error(message: Tree, cause: Ident): Tree = EmptyTree

    override def fatal(message: Tree, cause: Ident): Tree = EmptyTree

  }

}