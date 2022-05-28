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
    assertEquals(res, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  }

  test("bitcount") {
    val codePoint = 0x1f349
    val scx       = Scx(new String(Character.toChars(codePoint)))
    import scx.Utf8bits.*
    assertEquals(highest(Integer.parseInt("11100000", 2), 4), 3)
    assertEquals(highest(scx.peek, 4), 4)

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

    val ap = "a".all.str
    val bp = "b".all.str
    val cp = "c".all.str

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

    val res = p.run0(Scx("abcaaabc"))
    assertEquals(res, 8)
  }

  test("until") {
    val ut = until("abc".any).min(1).rep.min(1).str

    val res = ut.run0(Scx("eeeeeee"))
    assertEquals(res, "eeeeeee")
  }

  test("choice") {
    try
      val as   = "a".all.rep.min(1).str
      val bs   = "b".all.rep.min(2).str
      val cs   = "c".all.rep.min(3).str
      val choi = choice(as, bs, cs)

      val res = choi.run0(Scx("bbaccc"))
      assertEquals(res, "bb")
    catch case e: ScipEx => throw IllegalStateException(e.getMessage)
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
