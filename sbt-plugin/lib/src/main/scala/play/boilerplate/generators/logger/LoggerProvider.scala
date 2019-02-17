package play.boilerplate.generators.logger

import treehugger.forest._
import definitions._
import treehuggerDSL._

trait LoggerProvider {

  val loggerValRef: Ident = REF("logger")

  def loggerDefs: Seq[Tree]

  def clientTraceLoggerDef: Tree = LoggerProvider.defaultClientTraceLogger(this)

  def controllerTraceLoggerDef: Tree = LoggerProvider.defaultControllerTraceLogger(this)

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

  def defaultTraceLogger(className: String, provider: LoggerProvider): Tree = NEW(ANONDEF(RootClass.newClass(className)) := BLOCK(
    // override protected def errorInternal(msg: => String, error: => Throwable): Unit = ()
    DEF("errorInternal", UnitClass)
      .withFlags(Flags.OVERRIDE)
      .withParams(PARAM("msg", TYPE_BYNAME(StringClass)).empty, PARAM("error", TYPE_BYNAME(ThrowableClass)).empty) :=
      BLOCK {
        provider.error(REF("msg"), REF("error"))
      },
    // override protected def traceInternal(msg: => String): Unit = ()
    DEF("traceInternal", UnitClass)
      .withFlags(Flags.OVERRIDE)
      .withParams(PARAM("msg", TYPE_BYNAME(StringClass)).empty) :=
      BLOCK {
        provider.trace(REF("msg"))
      }
  ))

  def defaultClientTraceLogger(provider: LoggerProvider): Tree = defaultTraceLogger("ClientTraceLogger.Default", provider)

  def defaultControllerTraceLogger(provider: LoggerProvider): Tree = defaultTraceLogger("ControllerTraceLogger.Default", provider)

  def defaultPlayLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Seq(
      VAL("logger") := REF("Logger") APPLY (THIS DOT "getClass" DOT "getName")
    )

    override def imports: Seq[Import] = IMPORT(REF("play.api"), "Logger") :: Nil

    override def parents: Seq[Type] = Nil

    override def selfTypes: Seq[Type] = Nil

    override def trace(message: Tree): Tree = loggerValRef DOT "trace" APPLY message

    override def debug(message: Tree): Tree = loggerValRef DOT "debug" APPLY message

    override def info(message: Tree): Tree = loggerValRef DOT "info" APPLY message

    override def warning(message: Tree): Tree = loggerValRef DOT "warn" APPLY message

    override def warning(message: Tree, cause: Ident): Tree = loggerValRef DOT "warn" APPLY (message, cause)

    override def error(message: Tree, cause: Ident): Tree = loggerValRef DOT "error" APPLY (message, cause)

    override def fatal(message: Tree, cause: Ident): Tree = error(message, cause)

  }

  def withoutLogger: LoggerProvider = new LoggerProvider {

    override def loggerDefs: Seq[Tree] = Nil

    override def clientTraceLoggerDef: Tree = REF("ClientTraceLogger") DOT "NoLogger"

    override def controllerTraceLoggerDef: Tree = REF("ControllerTraceLogger") DOT "NoLogger"

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