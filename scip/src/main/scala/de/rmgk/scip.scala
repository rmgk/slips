package de.rmgk

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.{switch, tailrec}
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.compiletime.erasedValue
import scala.quoted.*
import scala.util.NotGiven
import scala.util.control.ControlThrowable
import scala.Tuple.*
import scala.deriving.Mirror
import scala.compiletime.summonFrom

object scip {

  trait ScipEx extends ControlThrowable

  case class Scx(
      val input: Array[Byte],
      var index: Int,
      var depth: Int,
      var lastFail: Int,
      var reason: String,
      val tracing: Boolean,
  ) {

    object ScipExInstance extends ScipEx {
      override def getMessage: String =
        s"$reason: ${debugat(index)}"
    }

    def fail(msg: String): Nothing =
      reason = msg
      lastFail = index
      throw ScipExInstance

    def ahead(i: Int): Byte = input(index + i)

    def slice(i: Int): IndexedSeqView[Byte] = input.view.slice(index, index + i)
    def debugat(i: Int): String = s"${index}»${input.view.slice(i, i + 12).str.replaceAll("\\n", "\\\\n")}«"

    def available: Int = input.length - index

    def assertAvailable(len: Int): Unit =
      val rem = input.length - index - len
      if rem < 0 then fail("unavailable")

    val EOT: Byte = 3.toByte

    def readSafe: Byte = if index < input.length then input(index) else EOT

    inline def peek: Byte = input(index)

    def next: Boolean =
      if index < input.length
      then { index += 1; true }
      else false

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
    def apply(s: String): Scx = Scx(s.getBytes(UTF_8), index = 0, depth = 0, lastFail = -1, reason = "", tracing = true)
  }

  class Scip[+A](val run0: Scx => A)

  object Scip {
    inline def apply[A](inline run: Scx ?=> A) = new Scip(run(using _))
  }

  inline def scx(using inline scx0: Scx): scx0.type = scx0

  type FlatConcat[A, B] = A match
    case Unit => B
    case _ => B match
        case Unit    => A
        case b *: bs => A *: b *: bs
        case _       => (A, B)

  inline def flatConcat[A, B](a: A, b: B): FlatConcat[A, B] =
    summonFrom {
      case given (A =:= Unit)  => b.asInstanceOf[FlatConcat[A, B]]
      case given (B =:= Unit)  => a.asInstanceOf[FlatConcat[A, B]]
      case given (B <:< Tuple) => (a *: b).asInstanceOf[FlatConcat[A, B]]
      case _                   => (a, b).asInstanceOf[FlatConcat[A, B]]
    }

  extension [A](inline scip: Scip[A]) {
    inline def run(using inline scx: Scx): A                        = scip.run0(scx)
    inline def ~:[B](inline other: Scip[B]): Scip[FlatConcat[A, B]] = Scip { flatConcat(scip.run, other.run) }
    inline def ~[B](inline other: Scip[B]): Scip[Unit]              = Scip { { scip.run; other.run } }
    inline def <~[B](inline other: Scip[B]): Scip[A]                = Scip { { val a = scip.run; other.run; a } }
    inline def ~>[B](inline other: Scip[B]): Scip[B]                = Scip { { scip.run; other.run } }
    inline def <~>[B](inline other: Scip[B]): Scip[(A, B)]          = Scip { (scip.run, other.run) }

    inline def opt: Scip[Option[A]] = Scip {
      scip.map(Some.apply).orElse(Option.empty).run
    }
    inline def capture: Scip[IndexedSeqView[Byte]] = Scip {
      val start = scx.index
      scip.run
      val end = scx.index
      scx.input.view.slice(start, end)
    }
    inline def map[B](inline f: A => B): Scip[B]           = Scip { f(scip.run) }
    inline def flatMap[B](inline f: A => Scip[B]): Scip[B] = map(f).flatten
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
    inline def attempt: Scip[Boolean] = scip.map(_ => true).orElse(false)

    inline def drop: Scip[Unit] = scip.map(_ => ())

    inline def list(inline sep: Scip[Unit]): Scip[List[A]] = Scip {
      val acc = ListBuffer.empty[A]
      var resultIndex = scx.index
      try
        var continue = true
        while continue do
          val start = scx.index
          val res   = scip.run
          resultIndex = scx.index
          acc.addOne(res)
          sep.run
          continue = start < scx.index
        acc.toList
      catch
        case e: ScipEx =>
          acc.toList
      finally scx.index = resultIndex
    }

    inline def require(inline check: A => Boolean): Scip[A] = Scip {
      val res = scip.run
      if check(res) then res else scx.fail(s"require")
    }

    inline def trace(inline name: String): Scip[A] = Scip {
      if !scx.tracing then scip.run
      else
        println(" " * scx.depth * 2 + s"+ $name ${scx.debugat(scx.index)}")
        scx.depth += 1
        try scip.run
        catch
          case e: ScipEx =>
            println(" " * (scx.depth - 1) * 2 + s"! $name (${e.getMessage})")
            throw e
        finally
          scx.depth -= 1
          println(" " * scx.depth * 2 + s"- $name ${scx.debugat(scx.index)}")
    }

  }

  extension [A](inline scip: Scip[Scip[A]]) inline def flatten: Scip[A] = Scip(scip.run.run)

  extension (inline scip: Scip[Boolean]) {
    inline def falseFail(msg: => String): Scip[Unit] = Scip {
      scip.run match
        case true  => ()
        case false => scx.fail(msg)
    }
    inline def rep: Scip[Int] = Scip {
      var matches = 0
      while scip.run do matches += 1
      matches
    }
  }
  extension (inline scip: Scip[Unit]) {
    inline def str: Scip[String] = scip.capture.map(_.str)
  }

  inline def whileRange(lo: Int, hi: Int): Scip[Unit]        = bpred(b => lo <= b && b <= hi).rep.drop
  inline def bpred(inline p: Byte => Boolean): Scip[Boolean] = Scip { scx.containsNext(p) }
  inline def cpred(inline p: Int => Boolean): Scip[Boolean] = Scip {
    scx.available > 0 && {
      val read = scx.intPred(p)
      scx.index += read
      read > 0
    }
  }

  inline def until(inline end: Scip[Boolean]): Scip[Int] = Scip {
    val start = scx.index
    while !end.lookahead.run && scx.next do ()
    start - scx.index
  }

  inline def whitespace: Scip[Unit] = cpred(Character.isWhitespace).rep.require(_ > 0).drop

  inline def choice[T](inline alternatives: Scip[T]*): Scip[T] = ${ choiceImpl('alternatives) }

  def choiceImpl[T: Type](alternatives: Expr[Seq[Scip[T]]])(using quotes: Quotes): Expr[Scip[T]] = {
    import quotes.reflect.*
    alternatives match
      case Varargs(args) => '{
          Scip { scx ?=>
            ${
              args.foldRight[Expr[T]]('{ scx.fail("choice") }) { (next: Expr[Scip[T]], acc) =>
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

  extension (isv: IndexedSeqView[Byte]) {
    def str: String = new String(isv.toArray, UTF_8)
  }

  extension (s: String) {
    inline def scip: Scip[Unit]   = ${ stringMatchImpl('s) }
    inline def any: Scip[Boolean] = ${ stringAltImpl('s) }
  }
  def stringMatchImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Unit]] =
    import quotes.reflect.*
    s.value match
      case None =>
        report.warning(s"value is not constant", s)
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

  def stringAltImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Boolean]] =
    import quotes.reflect.*
    s.value match
      case None =>
        report.errorAndAbort(s"value is not constant", s)
      case Some(v) => bytesAltImpl(v.getBytes(UTF_8))

  def bytesAltImpl(bytes: Array[Byte])(using quotes: Quotes): Expr[Scip[Boolean]] = {
    import quotes.reflect.*
    '{
      Scip { (scx: Scx) ?=>
        if (scx.available <= 0) false
        else ${
          val alternatives = Alternatives(bytes.sorted.distinct.toList.map { b => Expr(b).asTerm })
          Match(
            '{ scx.peek }.asTerm,
            List(
              CaseDef(alternatives, None, '{ scx.index += 1; true }.asTerm),
              CaseDef(Wildcard(), None, '{ false }.asTerm)
            )
          ).asExprOf[Boolean]
        }
      }
    }
  }

  inline def exact(b: String): Scip[Unit] = exact(b.getBytes(StandardCharsets.UTF_8))

  inline def exact(b: Array[Byte]): Scip[Unit] = Scip {
    val len = b.length
    scx.assertAvailable(len)
    var i = 0
    while i < len
    do
      if b(i) != scx.ahead(i) then scx.fail(s"exact »${b.view.str}«")
      i += 1
    scx.index += len
  }

}
