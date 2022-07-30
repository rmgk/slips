package de.rmgk

import org.scalacheck.*
import org.scalacheck.Prop.*
import de.rmgk.options.*

import java.nio.file.{Path, Paths}

class OptionsTest extends munit.FunSuite {

  case class CliArgs(first: Argument[Int, List, "number", "test argument", OptStyle.Positional] = Argument(),
                     second: Argument[Path, Required, "path", "path argument", OptStyle.Named] = Argument(),
                     defaulting: Argument[(Int, String), Option, "pair", "pair argument", OptStyle.Named] = Argument(Some(Some(2, "test"))))

  test("basic") {
    val p = options.makeParser[CliArgs]("test")
    val res = scopt.OParser.parse(p, Seq("42", "--second", "/a/b", "32"), CliArgs())
    assertEquals(res.get.first.value, List(42, 32))
    assertEquals(res.get.second.value, Paths.get("/a/b"))
    assertEquals(res.get.defaulting.value, Some(2, "test"))
  }

}

