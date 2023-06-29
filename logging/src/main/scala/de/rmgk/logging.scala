package de.rmgk

import de.rmgk.logging.Level.*

object logging {

  /** Logger using Strings as tags */
  object Logger {
    def apply(level: Level = Info, logPrinter: LogPrinter = noTracing): Logger =
      new Logger(level, logPrinter)

    lazy val tracing: DefaultLogPrinter   = new DefaultLogPrinter(printPosition = true, verboseInfo = false)
    lazy val noTracing: DefaultLogPrinter = new DefaultLogPrinter(printPosition = false, verboseInfo = false)
  }

  /** Convenience methods for logging at certain levels.
    * Tries to inline log calls so level check happens before method call.
    *
    * @param minLevel      minimum level that gets printed
    * @param logPrinter handles printing and formatting the log messages
    */
  case class Logger(
      minLevel: Level,
      logPrinter: LogPrinter
  ) {
    inline def trace[T: Loggable](inline m: String, info: T)(using Context): Unit = log(Trace, m, info)
    inline def info[T: Loggable](inline m: String, info: T)(using Context): Unit  = log(Info, m, info)
    inline def warn[T: Loggable](inline m: String, info: T)(using Context): Unit  = log(Warn, m, info)
    inline def trace(inline m: String)(using Context): Unit                       = log(Trace, m, null)
    inline def info(inline m: String)(using Context): Unit                        = log(Info, m, null)
    inline def warn(inline m: String)(using Context): Unit                        = log(Warn, m, null)

    inline def log[T](inline level: Level, inline message: String, info: T)(using c: Context, log: Loggable[T]): Unit =
      if (level.value >= minLevel.value) logPrinter.print(LogLine(level, message, info, log, c))

  }

  case class LogLine[T](level: Level, message: String, info: T, loggable: Loggable[T], context: Context)

  trait Loggable[T]:
    def normal(v: T): String
    def verbose(v: T): String = normal(v)
  object Loggable:
    given nullLoggable: Loggable[Null] with
      def normal(v: Null) = ""
    given stringLoggable: Loggable[String] with
      def normal(v: String) = s"»${v}«"
    def toStringLoggable[T]: Loggable[T] = new Loggable[T]:
      def normal(v: T) = v.toString

  case class Context(file: sourcecode.File, line: sourcecode.Line, enclosing: sourcecode.Enclosing)
  object Context {
    given fromImplicit(using file: sourcecode.File, line: sourcecode.Line, enclosing: sourcecode.Enclosing): Context =
      Context(file, line, enclosing)
  }

  /** Existing log levels, each has a corresponding method in the [[Logger]] */
  sealed class Level(final val value: Int)
  object Level {
    object Trace extends Level(0)
    object Info  extends Level(2)
    object Warn  extends Level(3)
  }

  /** The [[LogPrinter]] decides how messages are formatted */
  trait LogPrinter {
    def print[T](line: LogLine[T]): Unit
  }

  /** Prints to stdout.
    * Prints the level if it is not [[Level.Info]].
    * Prints non empty tags.
    *
    * @param printPosition add file and line number to log output if true
    */
  class DefaultLogPrinter(val printPosition: Boolean, val verboseInfo: Boolean) extends LogPrinter {

    def print[T](logLine: LogLine[T]): Unit = {
      println(format(logLine))
    }

    def format[T](logLine: LogLine[T]): String = {
      val trl = traceline(logLine.context)
      val infoline = formatInfo(logLine)
      s"${logLine.message}${infoline}$trl"
    }

    def formatInfo[T](logLine: LogLine[T]): String = {
      if logLine.info == null
      then ""
      else
        val res = if verboseInfo
        then logLine.loggable.verbose(logLine.info)
        else logLine.loggable.normal(logLine.info)
        s": »$res«"
    }

    def traceline(c: Context): String = {
      if (printPosition) {
        val fstring = c.file.value
        val last    = fstring.lastIndexOf('/') + 1
        s".(${fstring.substring(last)}:${c.line.value})[${c.enclosing.value}]"
      } else ""
    }
  }

}
