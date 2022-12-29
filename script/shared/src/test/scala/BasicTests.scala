import de.rmgk.script.{RunnableParts, process, run}

import java.io.IOException
import java.nio.file.Paths

class BasicTests extends munit.FunSuite {
  test("list files") {
    val path = Paths.get(".").toAbsolutePath
    val res  = process"ls ${path.toString}".run()
    assert(res.isRight)
  }

  test("no splitting inner strings") {
    try
      val res = process"${"ls -al"}".run()
      assert(res.isLeft, "inner strings are not split")
    catch
      case e: IOException => () // jvm throws an exception, thats fine
  }

  test("sequence arguments") {
    val path = Paths.get(".").toAbsolutePath
    val res  = process"${List[RunnableParts]("ls", path)}".run()
    assert(res.isRight)
  }

}
