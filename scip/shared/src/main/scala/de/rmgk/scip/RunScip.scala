package scitzen.scip

import scitzen.scip.Scip.*

import java.nio.charset.StandardCharsets

object RunScip {

  inline def directive = Scip {
    ":".parse.run
    val directive = characters.capture.str.run
    "{".parse.run
    val args = repeat(characters.capture.str, ";".parse ~ whitespace).run
    "}".parse.run
    (directive, args)
  }

  def header = Scip {
    "= ".parse.run
    until("\n".parse.lookahead).capture.str.run
  }

  def main(args: Array[String]): Unit = {

    given Scx = Scx("= Header\n:directive{one; two; three}")

    println(header.run)
    println("\n".parse.run)
    println(directive.run)


    println(printCode(choice("a".parse, "b".parse, "c".parse).capture.str.run(using Scx("b"))))

  }

}
