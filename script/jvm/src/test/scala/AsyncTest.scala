import de.rmgk.script.{extensions, ProcessResultException}
import de.rmgk.delay.{Async, extensions}

import java.io.IOException
import java.nio.file.Paths
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

class AsyncTest extends munit.FunSuite {
  test("list files") {
    Async {
      val path = Paths.get(".").nn.toAbsolutePath
      val res  = process"ls ${path.toString}".asyncResult.bind
    }.runToFuture
  }

  test("no splitting inner strings") {
    Async {
      val res = process"${"ls -al"}".asyncResult.bind
      assert(false, "inner strings are not split")

    }.runToFuture.recover {
      // jvm and native throw different exceptions, that’s fine
      case e: (IOException | ProcessResultException) =>
        Future.successful(())
    }
  }

  test("sequence arguments") {
    Async[Unit] {
      import scala.language.unsafeNulls
      val path = Paths.get(".").toAbsolutePath
      val res  = process"${List("ls", path)}".asyncResult.bind
    }.runToFuture(using ())
  }

}
