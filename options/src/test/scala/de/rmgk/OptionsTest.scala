package de.rmgk

import de.rmgk.options.*
import org.scalacheck.*

import java.nio.file.Path

class OptionsTest extends munit.FunSuite {

  test("basic"):

    val a = Argument[String]("file", Style.Named, "the important file", "/tmp")
    val b = Argument[String]("content", Style.Positional, "random content")

    val res = parseArguments(List("--file", "/a/test/", "this is a test")) {
      (a.value, b.value, Argument[String]("hey-chala", Style.Named).value)
    }
    assert(res.inner.isLeft, res)

  test("advanced"):
    val res = parseArguments(List("17")) {
      val x = 5
      Argument[Int]("test", Style.Positional).value
      x
    }

    assertEquals(res.inner, Right(5))

}
