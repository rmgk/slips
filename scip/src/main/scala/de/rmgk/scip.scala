package de.rmgk

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.{switch, tailrec}
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.compiletime.erasedValue
import scala.quoted.*
import scala.util.NotGiven
import scala.util.control.ControlThrowable

object scip {

  trait ScipEx extends ControlThrowable

  case class Scx(
      val input: Array[Byte],
      var index: Int,
      var goodIndex: Int,
      var depth: Int,
  ) {

    object ScipExInstance extends ScipEx {
      var message                     = () => "no error"
      override def getMessage: String = message()
    }

    def fail(msg: => String): Nothing =
      def myMsg() = s"$msg at: »${input.view.slice(index, index + 24).str}«"
      // println(myMsg())
      ScipExInstance.message = myMsg
      throw ScipExInstance

    def ahead(i: Int): Byte = input(index + i)

    def slice(i: Int): IndexedSeqView[Byte] = input.view.slice(index, index + 1)

    def available: Int = input.length - index

    def assertAvailable(len: Int): Unit =
      val rem = input.length - index - len
      if rem < 0 then fail(s"end of input reached, but ${-rem} more bytes requested")

    val EOT: Byte = 3.toByte

    def readSafe: Byte = if index < input.length then input(index) else EOT

    inline def peek: Byte = input(index)

    def next: Boolean =
      index += 1
      index < input.length

    inline def containsNext(inline p: Byte => Boolean): Boolean =
      index < input.length && p(peek) && { index += 1; true }

    def assertNext(b: Byte): Unit =
      assertAt(0, b)
      index += 1

    def assertAt(i: Int, b: Byte): Unit =
      val next = index + i
      (next < input.length && input(next) == b) || fail(s"expected $b")

    inline def intPred(inline p: Int => Boolean): Int = {
      val b = peek & 0xff
      if (b & Utf8bits.maxBit) == 0 then if p(b) then 1 else 0
      else
        val count = highest4(b)
        val v = (count: @switch) match
          case 2 => parse2(b)
          case 3 => parse3(b)
          case 4 => parse4(b)
        if (p(v)) count else 0
    }

    private def parse2(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 5), 1, 2)
    private def parse3(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 4), 1, 3)
    private def parse4(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 3), 1, 4)
    private def highest4(b: Int)    = Utf8bits.highest(b, 4)

    object Utf8bits {

      inline val maxBit: 128                               = 1 << 7
      inline def lowest(inline b: Int, inline n: Int): Int = b & ((1 << n) - 1)
      inline def bitAt(b: Int, inline pos: Int): Int       = (b & maxBit) >>> (7 - pos)
      inline def addLowest(b: Int, acc: Int, n: Int): Int  = lowest(b, n) | (acc << n)
      inline def grab(acc: Int, inline pos: Int, inline max: Int): Int =
        inline if (pos >= max) then acc
        else grab(addLowest(ahead(pos) & 0xff, acc, 6), pos + 1, max)
      inline def highest(b: Int, inline depth: Int): Int =
        inline if depth <= 0 then 0
        else
          val isSet: Int = ((b & maxBit) >>> 7)
          isSet + isSet * highest(b << 1, depth - 1)
    }
  }

  object Scx {
    def apply(s: String): Scx = Scx(s.getBytes(UTF_8), 0, 0, 0)
  }

  class Scip[+A](val run0: Scx => A)

  object Scip {
    inline def apply[A](inline run: Scx ?=> A) = new Scip(run(using _))
  }

  inline def scx(using inline scx0: Scx): scx0.type = scx0

  extension [A](inline scip: Scip[A]) {
    inline def run(using inline scx: Scx): A = scip.run0(scx)
    inline def ~[B](inline other: Scip[B]): Scip[Unit] = Scip {
      scip.run
      other.run
    }
    inline def opt: Scip[Option[A]] = Scip {
      scip.map(Some.apply).orElse(Option.empty).run
    }
    inline def capture: Scip[IndexedSeqView[Byte]] = Scip {
      val start = scx.index
      scip.run
      val end = scx.index
      scx.input.view.slice(start, end)
    }
    inline def map[B](inline f: A => B): Scip[B] = Scip { f(scip.run) }
    inline def accept: Scip[A] = Scip {
      val res = scip.run
      scx.goodIndex = scx.index
      res
    }
    inline def parseNonEmpty: Scip[A] = Scip {
      val start = scx.index
      val res   = scip.run
      if start == scx.index then scx.fail("parsed nothing")
      res
    }
    inline def orElse[B >: A](inline b: B): Scip[B] = Scip {
      val start = scx.index
      try scip.run
      catch
        case e: ScipEx =>
          scx.index = start
          b
    }
    inline def orNull: Scip[A | Null] = scip.orElse[A | Null](null)
    inline def lookahead: Scip[A] = Scip {
      val start = scx.index
      try scip.run
      finally scx.index = start
    }
    inline def ? : Scip[Boolean] = scip.map(_ => true).orElse(false)
  }

  inline def whileRange(lo: Int, hi: Int): Scip[Unit] = whilePred(b => lo <= b && b <= hi)
  inline def pred(inline p: Int => Boolean): Scip[Boolean] = Scip {
    scx.available > 0 && {
      val read = scx.intPred(p)
      scx.index += read
      read > 0
    }
  }

  extension (s: String) inline def scip: Scip[Unit] = ${ stringMatchImpl('s) }
  def stringMatchImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Unit]] =
    import quotes.reflect.*
    s.value match
      case None =>
        // report.warning(s"value is not constant", s)
        '{ exact($s.getBytes(UTF_8)) }
      case Some(v) => bytesMatchImpl(v.getBytes(UTF_8))

  def bytesMatchImpl(bytes: Array[Byte])(using quotes: Quotes): Expr[Scip[Unit]] = {
    import quotes.reflect.*
    if (bytes.length > 4) then '{ exact(${ Expr(bytes) }) }
    else
      '{
        Scip { (scx: Scx) ?=>
          ${
            val stmts: List[Statement] = bytes.iterator.map(b => '{ scx.assertNext(${ Expr(b) }) }.asTerm).toList
            val (start, last)          = stmts.splitAt(stmts.length - 1)
            (if start.isEmpty then last.head
             else Block(start, last.head.asInstanceOf[Term])).asExprOf[Unit]
          }
        }
      }
  }

  inline def exact(b: Array[Byte]): Scip[Unit] = Scip {
    val len = b.length
    scx.assertAvailable(len)
    var i = 0
    while i < len
    do
      if b(i) != scx.ahead(i) then scx.fail(s"input ahead »${scx.slice(len).str}« did not match »${b.view.str}« ")
      i += 1
    scx.index += len
  }

  inline def whilePred(inline p: Int => Boolean): Scip[Unit] = Scip {
    var matches = 0
    while pred(p).run do matches += 1
    if matches == 0 then scx.fail("must match at least once")
  }

  inline def whitespace: Scip[Unit] = whilePred(Character.isWhitespace)

  inline def choice[T](inline alternatives: Scip[T]*): Scip[T] = ${ choiceImpl('alternatives) }

  def choiceImpl[T: Type](alternatives: Expr[Seq[Scip[T]]])(using quotes: Quotes): Expr[Scip[T]] = {
    import quotes.reflect.*
    alternatives match
      case Varargs(args) => '{
          Scip { scx ?=>
            ${
              args.foldRight[Expr[T]]('{ scx.fail("no alternative matched") }) { (next: Expr[Scip[T]], acc) =>
                '{
                  $next.orNull.run(using scx) match
                    case null => $acc
                    case res  => res.asInstanceOf[T]
                }
              }
            }
          }
        }
  }

  inline def until(inline end: Scip[Any]): Scip[Unit] = Scip {
    while !end.?.lookahead.run && scx.next do ()
  }

  extension (isv: IndexedSeqView[Byte]) {
    def str: String = new String(isv.toArray, UTF_8)
  }
  extension (inline isv: Scip[IndexedSeqView[Byte]]) {
    inline def str: Scip[String] = isv.map(_.str)
  }
  extension (inline scip: Scip[Unit]) {
    inline def ! : Scip[String] = scip.capture.str
  }

  inline def unitOpt[T, A](inline normal: A): A =
    inline erasedValue[T] match
      case _: Unit => null.asInstanceOf[A]
      case other   => normal

  inline def repeat[A](inline it: Scip[A], inline sep: Scip[Any], min: Int): Scip[List[A]] = Scip {
    var continue = true
    val acc      = unitOpt[A, ListBuffer[A]](ListBuffer.empty[A])
    var count    = 0
    while continue do
      try
        val res = it.run
        count += 1
        unitOpt[A, ListBuffer[A]](acc.addOne(res))
        sep.run
      catch case e: ScipEx => continue = false
    val res = unitOpt[A, List[A]](acc.toList)
    if (count < min) scx.fail(s"repeat $min")
    if res == null then Nil else res
  }

}
