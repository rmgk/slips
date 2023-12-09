package de.rmgk

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

implicit object script:

  def deleteRecursive(path: Path): Unit =
    import scala.language.unsafeNulls
    if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)
    then Files.list(path).iterator().asScala.foreach(deleteRecursive)
    Files.delete(path)

  class ProcessResultException(val code: Int) extends Exception

  extension (p: Process)

    def output(): String =
      import scala.language.unsafeNulls
      val is = p.getInputStream
      try new String(is.readAllBytes, StandardCharsets.UTF_8)
      finally is.close()

    @throws[ProcessResultException]
    def ! : p.type =
      val code = p.exitValue()
      if code != 0
      then throw ProcessResultException(code)
      else p

  extension (pb: ProcessBuilder)

    inline def run(): Process =
      val proc = pb.start().nn
      proc.waitFor()
      proc

    def runOutput(): String =
      import scala.language.unsafeNulls
      pb.inheritIO().redirectOutput(Redirect.PIPE).run().!.output()

    def runInherit(): Process =
      import scala.language.unsafeNulls
      pb.inheritIO().run().!

  end extension

  type CommandPart = String | Path | Long | Int | Char
  extension (sc: StringContext)
    def process(args: (CommandPart | Iterable[CommandPart])*): ProcessBuilder =
      import scala.language.unsafeNulls
      val components = sc.parts.iterator.zipAll(args, "", List.empty[String]).flatMap: (part, arg) =>
        val parts = arg match
          case s: Iterable[_]     => s.map(_.toString)
          case other: CommandPart => Seq(other.toString())
        part.split("\\s").iterator.concat(parts)
      .filter(s => !s.isBlank).toVector
      new ProcessBuilder(components.asJava)
