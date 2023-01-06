import de.rmgk.script.{extensions, RunnableParts}

import java.io.IOException
import java.nio.file.Paths

class BasicTests extends munit.FunSuite {
  test("list files") {
    import scala.language.unsafeNulls
    val path = Paths.get(".").toAbsolutePath
    val res  = process"ls ${path.toString}".runResult()
    assert(res.isRight)
  }

  test("no splitting inner strings") {
    try
      val res = process"${"ls -al"}".runResult()
      assert(res.isLeft, "inner strings are not split")
    catch
      case e: IOException => () // jvm throws an exception, thats fine
  }

  test("sequence arguments") {
    import scala.language.unsafeNulls
    val path = Paths.get(".").toAbsolutePath
    val res  = process"${List[RunnableParts]("ls", path)}".runResult()
    assert(res.isRight)
  }

}
