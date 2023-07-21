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
    //res.printHelp()
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


  test("subcommands"):
    val res = parseArguments(List("test", "--file", "/a/test")):

      val sc1 = subcommand("run", "just run"):
        val b: String = named("--output", "output path").value
      .value

      val sc2 = subcommand("test", "run tests"):
        val a: String = named("--file", "path to test").value
        a
      .value



      (sc1, sc2)

    assertEquals(res.inner, Right((None,Some("/a/test"))))


  test("order test"):
    val res = parseArguments(List("a", "b", "c", "d")):
      val a = positional[String]("a", "a").value
      val b = positional[String]("b", "b").value
      val c = positional[String]("c", "c").value
      val d = positional[String]("d", "d").value
      (a, b, c, d)

    assertEquals(res.inner, Right(("a", "b", "c", "d")))
}
