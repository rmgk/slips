import de.rmgk.scip.*

class SimpleTest extends munit.FunSuite {

  test("chain") {
    given scx: Scx = Scx("abc")
    val res        = ("a".scip ~ "b".scip ~ "c".scip).capture.str.run
    assertEquals(scx.index, scx.input.length)
    assertEquals(res, "abc")
  }

  test("long match") {
    inline val input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    given scx: Scx   = Scx(input)
    val res          = input.scip.capture.str.run
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

    val dig = TimeParsers.digits.!.run(using Scx("1234"))

    assertEquals(dig, "1234")

    val date = ScitzenDate("2022", "04", "22")
    val res  = TimeParsers.date.run(using Scx(date.full))

    assertEquals(res, date)

    val dt = ScitzenDateTime(date = date, Some(ScitzenTime("10", "22", "55")))

    val dtparsed = TimeParsers.dateTime.run(using Scx(dt.full))

    assertEquals(dtparsed, dt)
  }

}

case class ScitzenDate(year: String, month: String, day: String) {
  def full = s"$year-$month-$day"
}
case class ScitzenTime(hour: String, minute: String, second: String) {
  def short = s"$hour:$minute"
  def full = s"$hour:$minute:$second"
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
  val digits: Scip[Unit] = whileRange('0', '9')
  val date: Scip[ScitzenDate] = Scip {
    val y = digits.!.run
    "-".scip.run
    val m = digits.!.run
    "-".scip.run
    val d = digits.!.run
    ScitzenDate(y, m, d)
  }
  val time = Scip {
    val res = ScitzenTime(
      digits.!.run,
      { ":".scip.run; digits.!.run },
      { ":".scip.run; digits.!.run }
    )
    (".".scip ~ digits).?.run
    res
  }
  val timezone = "+".scip ~ digits ~ ":".scip ~ digits
  val dateTime = Scip {
    val sdate = date.run
    val stime = Scip {
      println(scx.index)
      println(s"choice: »${choice("T".scip, whitespace).!.run}«")
      println(scx.index)
      val t = time.run
      timezone.?.run
      t
    }.opt.run

    println(s"$stime, »${scx.input.view.slice(scx.index, scx.index + 42).str}«")

    ScitzenDateTime(sdate, stime)
  }

}
