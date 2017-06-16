package play.boilerplate.generators.logger

import treehugger.forest._
import treehuggerDSL._

trait LoggerProvider {

  def loggerDefs: Seq[Tree]

  def imports: Seq[Import]

  def parents: Seq[Type]

  def selfTypes: Seq[Type]

  def trace(message: Ident, cause: Ident): Tree

  def debug(message: Ident, cause: Ident): Tree

  def info(message: Ident, cause: Ident): Tree

  def warning(message: Ident, cause: Ident): Tree

  def error(message: Ident, cause: Ident): Tree

  def fatal(message: Ident, cause: Ident): Tree

}

object LoggerProvider {

  def defaultPlayLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Seq(
      VAL("logger") := REF("Logger") APPLY (THIS DOT "getClass" DOT "getName")
    )

    override def imports: Seq[Import] = IMPORT("play.api", "Logger") :: Nil

    override def parents: Seq[Type] = Nil

    override def selfTypes: Seq[Type] = Nil

    override def trace(message: Ident, cause: Ident): Tree = REF("logger") DOT "trace" APPLY (message, cause)

    override def debug(message: Ident, cause: Ident): Tree = REF("logger") DOT "debug" APPLY (message, cause)

    override def info(message: Ident, cause: Ident): Tree = REF("logger") DOT "info" APPLY (message, cause)

    override def warning(message: Ident, cause: Ident): Tree = REF("logger") DOT "warn" APPLY (message, cause)

    override def error(message: Ident, cause: Ident): Tree = REF("logger") DOT "error" APPLY (message, cause)

    override def fatal(message: Ident, cause: Ident): Tree = error(message, cause)

  }

  def withoutLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Nil

    override def imports: Seq[Import] = Nil

    override def parents: Seq[Type] = Nil

    override def selfTypes: Seq[Type] = Nil

    override def trace(message: Ident, cause: Ident): Tree = EmptyTree

    override def debug(message: Ident, cause: Ident): Tree = EmptyTree

    override def info(message: Ident, cause: Ident): Tree = EmptyTree

    override def warning(message: Ident, cause: Ident): Tree = EmptyTree

    override def error(message: Ident, cause: Ident): Tree = EmptyTree

    override def fatal(message: Ident, cause: Ident): Tree = EmptyTree

  }

}