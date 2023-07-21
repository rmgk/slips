package de.rmgk

import de.rmgk.options.*
import org.scalacheck.*

import java.nio.file.Path

class OptionsTest extends munit.FunSuite {

  test("basic"):

    val a = named[String]("--file", "the important file", "/tmp")
    val b = positional[String]("content", "random content")

    val res = parseArguments(List("--file", "/a/test/", "this is a test")) {
      (a.value, b.value, named[String]("hey-chala", "some key").value)
    }
    res.printHelp()
    assert(res.inner.isLeft, res)

  test("advanced"):
    val res = parseArguments(List("17")) {
      val x = 5
      positional[Int]("int", "some arg").value
      x
    }

    assertEquals(res.inner, Right(5))


  test("success"):
    val res = parseArguments(List("--file", "/a/test/", "this is a test", "--verbose")):
      val a = named[String]("--file", "the important file", "/tmp").value
      val b = positional[String]("content", "random content").value
      val c = named[Boolean]("--verbose", "make output more verbose").value
      (a, b, c)

    assertEquals(res.inner, Right(("/a/test/", "this is a test", true)))

}
