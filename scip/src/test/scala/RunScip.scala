import java.nio.charset.StandardCharsets
import de.rmgk.scip.*
import de.rmgk.scip.Scip.*

object RunScip {

  inline def directive = Scip {
    ":".scip.run
    val directive = characters.capture.str.run
    "{".scip.run
    val args = repeat(characters.capture.str, ";".scip ~ whitespace).run
    "}".scip.run
    (directive, args)
  }

  def header = Scip {
    "= ".scip.run
    until("\n".scip.lookahead).capture.str.run
  }

  def main(args: Array[String]): Unit = {

    given Scx = Scx("= Header\n:directive{one; two; three}")

    println(header.run)
    println("\n".scip.run)
    println(directive.run)

    println(printCode(choice("a".scip, "b".scip, "c".scip).capture.str.run(using Scx("b"))))

  }

}
