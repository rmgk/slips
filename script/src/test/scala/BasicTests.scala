import de.rmgk.script

import de.rmgk.script.ProcessResultException

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.file.{Files, Path}

class BasicTests extends munit.FunSuite {
  test("list files") {
    import scala.language.unsafeNulls
    val path = Path.of(".").toAbsolutePath
    val res  = process"ls ${path.toString}".run()
  }

  test("no splitting inner strings") {
    try
      val res = process"${"ls -al"}".run().!
      assert(false, "should throw an exception")
    catch
      case e: ProcessResultException => () // native results in some non zero exit code
      case e: IOException            => () // jvm throws an exception before execution, that’s fine
  }

  test("sequence arguments") {
    import scala.language.unsafeNulls
    val path = Path.of(".").toAbsolutePath
    val res  = process"${List("ls", path)}".run()
  }

}
