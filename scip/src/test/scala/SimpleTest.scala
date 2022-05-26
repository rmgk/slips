import de.rmgk.scip.*

class SimpleTest extends munit.FunSuite {

  test("chain") {
    given scx: Scx = Scx("abc")
    val res = ("a".scip ~ "b".scip ~ "c".scip).capture.str.run
    assertEquals(scx.index, scx.input.length)
    assertEquals(res, "abc")
  }

  test("long match") {
    inline val input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    given scx: Scx = Scx(input)
    println(printCode(input.scip.capture.str.run))
    val res = input.scip.capture.str.run
    assertEquals(scx.index, scx.input.length)
    assertEquals(res, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  }

}
