import de.rmgk.script.{RunnableParts, asyncResult, process}
import de.rmgk.delay.Async

import java.io.IOException
import java.nio.file.Paths
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

class AsyncTest extends munit.FunSuite {
  test("list files") {
    Async {
      val path = Paths.get(".").toAbsolutePath
      val res  = process"ls ${path.toString}".asyncResult.bind
      assert(res.isRight)
    }.runToFuture
  }

  test("no splitting inner strings") {
    Async {
      val res = process"${"ls -al"}".asyncResult.bind
      assert(res.isLeft, "inner strings are not split")

    }.runToFuture.recover {
      case e: IOException =>
        // jvm throws an exception, that’s fine
        Future.successful(())
    }
  }

  test("sequence arguments") {
    Async {
      val path = Paths.get(".").toAbsolutePath
      val res  = process"${List[RunnableParts]("ls", path)}".asyncResult.bind
      assert(res.isRight)
    }.runToFuture
  }

}