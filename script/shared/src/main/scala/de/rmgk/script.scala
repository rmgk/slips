package de.rmgk.script

import de.rmgk.delay.*

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.Using

object ScriptUtil {
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
}

def streamToString(in: InputStream): String = {
  val bo = new ByteArrayOutputStream()
  ScriptUtil.transferTo(in, bo)
  bo.toString(StandardCharsets.UTF_8)
}

extension (path: Path)
  def readToString: String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

abstract class RunnableParts {
  def parts: IterableOnce[String]
}

object RunnableParts {
  given stringRunnable: Conversion[String, RunnableParts] = s =>
    new RunnableParts {
      override def parts = List(s)
    }
  given pathRunnable: Conversion[Path, RunnableParts] = p =>
    new RunnableParts {
      override def parts = List(p.toString)
    }
  given seqRunnable[A](using conv: Conversion[A, RunnableParts]): Conversion[Seq[A], RunnableParts] = p =>
    new RunnableParts {
      override def parts = p.flatMap(x => conv(x).parts)
    }
}

extension (pb: ProcessBuilder)
  def run(): Either[Int, String] = {
    val process = pb
      .inheritIO().redirectOutput(Redirect.PIPE)
      .start()
    val code = process.waitFor()

    if code != 0
    then Left(code)
    else Right(Using(process.getInputStream)(streamToString).get)
  }

extension (sc: StringContext)
  def process(args: RunnableParts*): ProcessBuilder = {
    val components = sc.parts.iterator.zipAll(args, "", List.empty[String]: RunnableParts).flatMap { (part, arg) =>
      part.split("\\s").iterator.concat(arg.parts)
    }.filter(s => !s.isBlank).toVector
    new ProcessBuilder(components.asJava)
  }
