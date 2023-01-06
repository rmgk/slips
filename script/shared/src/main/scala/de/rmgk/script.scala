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
  else Files.delete(path)
}

def streamToString(in: InputStream): String = {
  val bo = new ByteArrayOutputStream()
  transferTo(in, bo)
  bo.toString(StandardCharsets.UTF_8)
}

abstract class RunnableParts {
  def parts: IterableOnce[String]
}

object RunnableParts {
  given toStringRunnable: Conversion[String | Int | Long | Path, RunnableParts] = s =>
    new RunnableParts { override def parts = List(s.toString) }
  given seqRunnable[A](using conv: Conversion[A, RunnableParts]): Conversion[Seq[A], RunnableParts] = p =>
    new RunnableParts { override def parts = p.flatMap(x => conv(x).parts) }

  given seqIdRunnable: Conversion[Seq[RunnableParts], RunnableParts] = seqRunnable(using identity)
}

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

  extension (sc: StringContext)
    def process(args: RunnableParts*): ProcessBuilder = {
      val components = sc.parts.iterator.zipAll(args, "", List.empty[String]: RunnableParts).flatMap { (part, arg) =>
        import scala.language.unsafeNulls
        part.split("\\s").iterator.concat(arg.parts)
      }.filter(s => !s.isBlank).toVector
      new ProcessBuilder(components.asJava)
    }
