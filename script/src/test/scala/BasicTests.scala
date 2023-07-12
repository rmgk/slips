import de.rmgk.script.{extensions, ProcessResultException}

import java.io.IOException
import java.nio.file.Paths

class BasicTests extends munit.FunSuite {
  test("list files") {
    import scala.language.unsafeNulls
    val path = Paths.get(".").toAbsolutePath
    val res  = process"ls ${path.toString}".run()
  }

  test("no splitting inner strings") {
    try
      val res = process"${"ls -al"}".run()
      assert(false, "should throw an exception")
    catch
      case e: ProcessResultException => () // native results in some non zero exit code
      case e: IOException => () // jvm throws an exception before execution, thats fine
  }

  test("sequence arguments") {
    import scala.language.unsafeNulls
    val path = Paths.get(".").toAbsolutePath
    val res  = process"${List("ls", path)}".run()
  }

}
