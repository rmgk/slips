package de.rmgk.logging

import de.rmgk.logging.Level._


class Level(final val value: Int, final val prefix: String)
object Level {
  object Trace extends Level(0, "Trace: ")
  object Debug extends Level(1, "Debug: ")
  object Info extends Level(2, "")
  object Warn extends Level(3, "Warn : ")
  object Error extends Level(4, "Error: ")
}

case class Logger(tag: String = "",
                  level: Level = Trace,
                  tracing: Boolean = true,
                 ) {
  @inline final def trace(m: => Any)(implicit c: Context): Unit = log(Trace, m)
  @inline final def debug(m: => Any)(implicit c: Context): Unit = log(Debug, m)
  @inline final def info(m: => Any)(implicit c: Context): Unit = log(Info, m)
  @inline final def warn(m: => Any)(implicit c: Context): Unit = log(Warn, m)
  @inline final def error(m: => Any)(implicit c: Context): Unit = log(Error, m)

  @inline final def tracemsg(c: Context): String = {
    if (tracing) makeTrace(c) else ""
  }

  @inline final def makeTrace(c: Context): String = {
    val fstring = c.file.value
    val last = fstring.lastIndexOf('/') + 1
    s".(${fstring.substring(last)}:${c.line.value})"
  }

  @inline final def log(l: Level, m: => Any)(implicit c: Context): Unit =
    if (l.value >= level.value) Console.println(s"${l.prefix}$m [$tag]${tracemsg(c)}")
}

case class Context(file: sourcecode.File, line: sourcecode.Line)
object Context {
  @inline implicit def fromImplicit(implicit file: sourcecode.File, line: sourcecode.Line): Context = Context(file, line)
}
