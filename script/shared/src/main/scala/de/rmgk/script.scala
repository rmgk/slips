package de.rmgk.script

import de.rmgk.delay.*

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.Using

def transferTo(in: InputStream, out: OutputStream): Unit = {
  val bufferSize = 1024
  val buffer     = new Array[Byte](bufferSize)
  @tailrec def rec(): Unit =
    val read = in.read(buffer, 0, bufferSize)
    if (read >= 0)
      out.write(buffer, 0, read)
      rec()
  rec()
}

def deleteRecursive(path: Path): Unit = {
  if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)
  then Files.list(path).nn.iterator().nn.asScala.toSeq.foreach(deleteRecursive)
  Files.delete(path)
}

def streamToString(in: InputStream): String = {
  val bo = new ByteArrayOutputStream()
  transferTo(in, bo)
  bo.toString(StandardCharsets.UTF_8)
}

class ProcessResultException(code: Int) extends Exception

implicit object extensions:
  extension (path: Path)
    def readToString: String =
      // read string not present on native :(
      new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  extension (p: Process)
    @throws[ProcessResultException]
    def resultString(): String =
      val code = p.exitValue()
      if code != 0
      then throw ProcessResultException(code)
      else Using(p.getInputStream.nn)(streamToString).get

  extension (pb: ProcessBuilder)
    def scriptStart(): Process =
      import scala.language.unsafeNulls
      pb.inheritIO().redirectOutput(Redirect.PIPE).start()

    def run(): String =
      val process = pb.scriptStart()
      process.waitFor()
      process.resultString()

    def runPrint(): Unit = println(run())

    def asyncResult: Async[Any, String] = Async {
      val process = scriptStart().onExit().nn.toAsync.bind
      process.resultString()
    }
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
