package de.rmgk

import de.rmgk.options.*
import org.scalacheck.*

class OptionsTest extends munit.FunSuite {

  case class CliArgs(
      first: Argument[Int, List, Style.Positional] = Argument(_.unbounded().text("test argument").valueName("number")),
      second: Argument[String, Single, Style.Named] = Argument(_.optional().valueName("path").text("path argument")),
      defaulting: Argument[(Int, String), Option, Style.Named] =
        Argument(_.valueName("pair").text("pair argument"), Some(Some((2, "test")))),
      noflag: Argument[Unit, Flag, Style.Named] = Argument(_.text("test simple args")),
      flagish: Argument[Unit, Flag, Style.Named] = Argument(_.text("test simple args"))
  )

  test("basic") {

    val instance = CliArgs()

    val p   = options.makeParser[CliArgs]( instance, _.programName("test"))
    val res = scopt.OParser.parse(p, Seq("42", "--second", "/a/b", "32", "--flagish"), instance)
    assertEquals(res.get.first.value, List(42, 32))
    assertEquals(res.get.second.value, "/a/b")
    assertEquals(res.get.defaulting.value, Some(2, "test"))
    assertEquals(res.get.noflag.value, false)
    assertEquals(res.get.flagish.value, true)
  }

  case class CliArgsSub(
      sub: Subcommand[CliArgs] = Subcommand(CliArgs(), _.text("some subcommand"))
  )

  test("subcommand") {

    val instance = CliArgsSub()

    val p   = options.makeParser[CliArgsSub](instance, _.programName("test"))
    val res = scopt.OParser.parse(p, Seq("sub", "42", "--second", "/a/b", "32", "--flagish"), instance)
    assertEquals(res.get.sub.value.get.first.value, List(42, 32))
    assertEquals(res.get.sub.value.get.second.value, "/a/b")
    assertEquals(res.get.sub.value.get.defaulting.value, Some(2, "test"))
    assertEquals(res.get.sub.value.get.noflag.value, false)
    assertEquals(res.get.sub.value.get.flagish.value, true)
  }

}
