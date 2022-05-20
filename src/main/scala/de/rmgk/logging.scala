package de.rmgk

import de.rmgk.logging.Level._

object logging {

  /** Logger using Strings as tags */
  type Logger = LoggerT[String]
  object Logger {
    def apply(tag: String = "", level: Level = Trace, logPrinter: LogPrinter[String] = noTracing): Logger =
      new LoggerT[String](tag, level, logPrinter)

    lazy val tracing: DefaultLogPrinter[String]   = new DefaultLogPrinter[String](tracing = true)
    lazy val noTracing: DefaultLogPrinter[String] = new DefaultLogPrinter[String](tracing = false)
  }

  /** Convenience methods for logging at certain levels.
    * Tries to inline log calls so level check happens before method call.
    *
    * @param tag        tag to log with, used by the printer
    * @param level      minimum level that gets printed
    * @param logPrinter handles printing and formatting the log messages
    * @tparam T tag type passed to the log printer
    */
  case class LoggerT[T](
      tag: T,
      level: Level = Trace,
      logPrinter: LogPrinter[T] = new DefaultLogPrinter[T](tracing = false)
  ) {
    @inline final def trace(m: => Any)(implicit c: Context): Unit = log(Trace, m)
    @inline final def debug(m: => Any)(implicit c: Context): Unit = log(Debug, m)
    @inline final def info(m: => Any)(implicit c: Context): Unit  = log(Info, m)
    @inline final def warn(m: => Any)(implicit c: Context): Unit  = log(Warn, m)
    @inline final def error(m: => Any)(implicit c: Context): Unit = log(Error, m)

    @inline final def log(l: Level, m: => Any)(implicit c: Context): Unit =
      if (l.value >= level.value) logPrinter.print(tag, l, m, c)

  }

  case class Context(file: sourcecode.File, line: sourcecode.Line)
  object Context {
    implicit def fromImplicit(implicit file: sourcecode.File, line: sourcecode.Line): Context = Context(file, line)
  }

  /** Existing log levels, each has a corresponding method in the [[Logger]] */
  sealed class Level(final val value: Int, final val prefix: String)
  object Level {
    object Trace  extends Level(0, "Trace: ")
    object Debug  extends Level(1, "Debug: ")
    object Info   extends Level(2, "Info : ")
    object Warn   extends Level(3, "Warn : ")
    object Error  extends Level(4, "Error: ")
    object Silent extends Level(Int.MaxValue, "Silent")
  }

  /** The [[LogPrinter]] decides how messages are formatted
    *
    * @tparam T type of the tag used by the logger
    */
  trait LogPrinter[T] {
    def print(tag: T, level: Level, message: => Any, context: Context): Unit
  }

  /** Prints to stdout.
    * Prints the level if it is not [[Info]].
    * Prints non empty tags.
    *
    * @param tracing add file and line number to log output if true
    * @tparam T type of the tag used by the logger
    */
  class DefaultLogPrinter[T](val tracing: Boolean) extends LogPrinter[T] {

    def print(tag: T, l: Level, m: => Any, c: Context): Unit = {
      println(logline(tag, l, m, c))
    }

    def logline(tag: T, l: Level, m: => Any, c: Context): String = {
      val tal = tagline(tag)
      val lel = levelline(l)
      val trl = traceline(c)
      s"$lel$m$tal$trl"
    }

    def tagline(tag: T): String = {
      val tagstring = tag.toString
      if (tagstring.nonEmpty) s" [$tag]" else ""
    }
    def levelline(l: Level): String = if (l == Info) "" else l.prefix
    def traceline(c: Context): String = {
      if (tracing) {
        val fstring = c.file.value
        val last    = fstring.lastIndexOf('/') + 1
        s".(${fstring.substring(last)}:${c.line.value})"
      } else ""
    }
  }

}
