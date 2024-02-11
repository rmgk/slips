import de.rmgk.scip.*

class SimpleTest extends munit.FunSuite {

  test("chain") {
    given scx: Scx = Scx("abc")
    val res        = ("a".all and "b".all and "c".all).str.run
    assertEquals(res, "abc")
    assertEquals(scx.index, scx.input.length)
  }

  test("long match") {
    inline val input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    given scx: Scx   = Scx(input)
    val res          = input.all.str.run
    assertEquals(scx.index, scx.input.length)
    assertEquals(res, input)
  }

  test("dynamic sequence") {
    inline val input = "abc"
    given scx: Scx   = Scx(input)
    val res          = seq(input).str.run
    assertEquals(scx.index, scx.input.length)
    assertEquals(res, input)

    val res2 = (seq("some stuff") and seq("[")).str.runInContext(Scx("some stuff[ eh"))
    assertEquals(res2, "some stuff[")

  }

  test("bitcount") {
    val codePoint = 0x1f349
    val scx       = Scx(new String(Character.toChars(codePoint)))
    import scx.Utf8bits.*
    assertEquals(scx.intPred { _ == codePoint }, 4)
  }

  test("time") {

    val dig = TimeParsers.digits.str.run(using Scx("1234"))

    assertEquals(dig, "1234")

    val date = ScitzenDate("2022", "04", "22")
    val res  = TimeParsers.date.run(using Scx(date.full))

    assertEquals(res, date)

    val dt = ScitzenDateTime(date = date, Some(ScitzenTime("10", "22", "55")))

    val dtparsed = TimeParsers.dateTime.run(using Scx(dt.full))

    assertEquals(dtparsed, dt)
  }

  test("sequence") {
    val as  = "a".all.rep
    val bs  = "b".all.rep
    val cs  = "c".all.rep
    val res = (as <~> bs).run(using Scx("aaaabbbbb"))
    assertEquals(res, (4, 5))

    val p3 = Scip((as.run, bs.run, cs.run))

    val res2 = p3.run(using Scx("aaaabbbbbcc"))

    assertEquals(res2, (4, 5, 2))

    val res3 = p3.run(using Scx("aaaabbbbbcc"))

    assertEquals(res3, res2)
  }

  test("flatten") {
    def as    = "a".all.rep
    def parse = as.flatMap { count => "b".all.rep.require(_ == count) }
    val res   = parse.run(using Scx("aaaabbbb"))
    assertEquals(res, 4)
  }

  test("flatmap") {

    inline def ap = "a".all.str
    inline def bp = "b".all.str
    inline def cp = "c".all.str

    inline def parse =
      for
        a <- ap
        b <- bp
        c <- cp
      yield (a, b, c)

    val res = parse.run(using Scx("abc"))
    assertEquals(res, ("a", "b", "c"))
  }

  test("alternatives") {

    inline def p = "abc".any.rep

    val res = p.runInContext(Scx("abcaaabc"))
    assertEquals(res, 8)
  }

  test("until") {
    val ut = until("abc".any).min(1).rep.min(1).str

    val res = ut.runInContext(Scx("eeeeeeebee"))
    assertEquals(res, "eeeeeee")

    val res2 = until(seq("]")).min(0).str.runInContext(Scx("test] meh"))
    assertEquals(res2, "test")

  }

  test("choice") {
    try
      val as   = "a".all.rep.min(1).str
      val bs   = "b".all.rep.min(2).str
      val cs   = "c".all.rep.min(3).str
      val choi = (as | bs | cs)

      val res = choi.runInContext(Scx("bbaccc"))
      assertEquals(res, "bb")
    catch case e: ScipEx => throw IllegalStateException(e.getMessage)
  }

  test("more than one byte alternatives") {
    inline def p = "äöü".any.rep.min(0).str

    val s   = "öööäääüüüüä"
    val res = p.runInContext(Scx(s))
    assertEquals(res, s)
  }

  test("multi string choice") {
    inline def p = choice("abc def 1", "abc hef 2", "abc kef 1").str
    val s        = "abc kef 1"

    val res =
      try p.runInContext(Scx(s))
      catch
        case e: Throwable =>
          e.printStackTrace()
          throw e
    assertEquals(res, s)
  }

  test("unicode + any") {
    inline def listSybol      = "•*".any
    val digits: Scip[Boolean] = cpred(Character.isDigit)
    val p                     = listSybol or digits
    val s                     = "…"
    val res                   = p.runInContext(Scx(s))
    assertEquals(res, false)
  }

  test("augment bytes") {

    val s = "123aβcABC"
    val p = "123".all.orFail ~> "aβc".all.str.augment(_.byteCount).augment(_.region)

    val res = p.runInContext(Scx(s))
    assertEquals(res, (("aβc", 4), (3, 7)))
  }


  test("repeat empty match") {

    val s = "123aβcABC"
    val p = Scip(true).repSafe

    val res = p.runInContext(Scx(s))
    assertEquals(res, 1)

  }

}

case class ScitzenDate(year: String, month: String, day: String) {
  def full = s"$year-$month-$day"
}
case class ScitzenTime(hour: String, minute: String, second: String) {
  def short = s"$hour:$minute"
  def full  = s"$hour:$minute:$second"
  def iso   = s"$hour:$minute:${second}Z"
}
case class ScitzenDateTime(date: ScitzenDate, timeO: Option[ScitzenTime]) {
  def timeAppend: String   = timeO.fold("")(st => s" ${st.full}")
  def full: String         = s"${date.full}$timeAppend"
  def dayTime: String      = s"${date.day}$timeAppend"
  def monthDayTime: String = s"${date.month}-$dayTime"
  def year: String         = date.year
  def iso: String          = s"${date.full}${timeO.fold("")(_.iso)}"
}
object ScitzenDateTime {
  implicit val ord: Ordering[ScitzenDateTime] = Ordering.by(_.full)
}

object TimeParsers {
  val digits: Scip[Boolean] = bpred(b => '0' <= b && b <= '9').rep.min(1)
  val date: Scip[ScitzenDate] = Scip {
    val y = digits.str.run
    "-".all.run
    val m = digits.str.run
    "-".all.run
    val d = digits.str.run
    ScitzenDate(y, m, d)
  }
  val time = Scip {
    val res = ScitzenTime(
      digits.str.run,
      (":".all ifso digits.str).run,
      (":".all ifso digits.str).run
    )
    (".".all.orFail ~> digits).attempt.run
    res
  }
  val timezone = "+".all.ifso(digits <~> (":".all ifso digits))
  val dateTime = Scip {
    val sdate = date.run
    val stime = Scip {
      ("T".all or cpred(Character.isWhitespace)).str.run
      val t = time.run
      timezone.attempt.run
      t
    }.opt.run
    ScitzenDateTime(sdate, stime)
  }

}
