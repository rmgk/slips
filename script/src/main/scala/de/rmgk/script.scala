package de.rmgk.script

import java.io.{ByteArrayOutputStream, InputStream}
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.Using

def deleteRecursive(path: Path): Unit =
  import scala.language.unsafeNulls
  if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)
  then Files.list(path).iterator().asScala.toSeq.foreach(deleteRecursive)
  Files.delete(path)

class ProcessResultException(val code: Int) extends Exception

implicit object syntax:

  extension (in: InputStream)
    def readToBAOS: ByteArrayOutputStream =
      val bo = new ByteArrayOutputStream(128)
      in.transferTo(bo)
      bo
    def readToByteArray: Array[Byte] = in.readToBAOS.toByteArray.nn
    def readToString: String         = in.readToBAOS.toString(StandardCharsets.UTF_8)

  extension (p: Process)
    @throws[ProcessResultException]
    def resultString(): String =
      val code = p.exitValue()
      if code != 0
      then throw ProcessResultException(code)
      else Using(p.getInputStream.nn)(_.readToString).get

  extension (pb: ProcessBuilder)
    def scriptStart(): Process =
      import scala.language.unsafeNulls
      pb.inheritIO().redirectOutput(Redirect.PIPE).start()

    def run(): String =
      val process = pb.scriptStart()
      process.waitFor()
      process.resultString()

    def runPrint(): Int =
      import scala.language.unsafeNulls
      pb.inheritIO().start().waitFor()

  end extension

  type CommandPart = String | Path | Long | Int | Char
  extension (sc: StringContext)
    def process(args: (CommandPart | Iterable[CommandPart])*): ProcessBuilder = {
      val components = sc.parts.iterator.zipAll(args, "", List.empty[String]).flatMap { (part, arg) =>
        import scala.language.unsafeNulls
        val parts = arg match
          case s: Iterable[_]     => s.map(_.toString)
          case other: CommandPart => Seq(other.toString())
        part.split("\\s").iterator.concat(parts)
      }.filter(s => !s.isBlank).toVector
      new ProcessBuilder(components.asJava)
    }
