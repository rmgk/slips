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

type CommandPart = String | Path | Long | Int | Char

given extensions: Object with
  extension (path: Path)
    def readToString: String =
      new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  extension (pb: ProcessBuilder)
    def runResult(): Either[Int, String] = {
      import scala.language.unsafeNulls
      val process = pb
        .inheritIO().redirectOutput(Redirect.PIPE)
        .start()
      val code = process.waitFor()

      if code != 0
      then Left(code)
      else Right(Using(process.getInputStream)(streamToString).get)
    }
    def run(): String =
      pb.runResult().toOption.getOrElse("")
    def runPrint(): Unit = println(run())

  extension (sc: StringContext)
    def process(args: (CommandPart | Seq[CommandPart])*): ProcessBuilder = {
      val components = sc.parts.iterator.zipAll(args, "", List.empty[String]).flatMap { (part, arg) =>
        import scala.language.unsafeNulls
        val parts = arg match
          case s: Seq[_]          => s.map(_.toString)
          case other: CommandPart => Seq(other.toString())
        part.split("\\s").iterator.concat(parts)
      }.filter(s => !s.isBlank).toVector
      new ProcessBuilder(components.asJava)
    }
