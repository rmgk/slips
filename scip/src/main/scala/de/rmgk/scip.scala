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
import scala.compiletime.constValueOpt

object scip {

  trait ScipEx extends ControlThrowable

  case class Scx(
      val input: Array[Byte],
      var index: Int,
      val maxpos: Int,
      var depth: Int,
      var lastFail: Int,
      var reason: String,
      val tracing: Boolean,
  ) {

    object ScipExInstance extends ScipEx {
      override def getMessage: String =
        s"$reason: ${debugat(index)}"
    }

    def debugat(i: Int): String = s"${index}»${str(i, i + 12).replaceAll("\\n", "\\\\n")}«"

    def str(l: Int, r: Int) = new String(input, l, math.min(r, maxpos) - l, UTF_8)

    def contains(bytes: Array[Byte]): Boolean =
      java.util.Arrays.equals(bytes, 0, bytes.length, input, index, math.min(index + bytes.length, maxpos))

    def available(min: Int): Boolean    = maxpos >= index + min
    def ahead(i: Int, b: Byte): Boolean = input(index + i) == b

    inline def intPred(inline p: Int => Boolean): Int = {
      val b = this.peek & 0xff
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
        else grab(addLowest(input(index + pos) & 0xff, acc, 6), pos + 1, max)
      inline def highest(b: Int, inline depth: Int): Int =
        inline if depth <= 0 then 0
        else
          val isSet: Int = ((b & maxBit) >>> 7)
          isSet + isSet * highest(b << 1, depth - 1)
    }
  }

  extension (inline scx: Scx) {

    inline def fail: Nothing =
      scx.lastFail = scx.index
      throw scx.ScipExInstance

    inline def peek: Byte = scx.input(scx.index)

    inline def next: Boolean =
      if scx.index < scx.maxpos
      then { scx.index += 1; true }
      else false

    inline def containsNext(inline p: Byte => Boolean): Boolean =
      scx.index < scx.maxpos && p(scx.peek) && { scx.index += 1; true }

  }

  object Scx {
    def apply(s: String): Scx =
      val b = s.getBytes(UTF_8)
      Scx(b, index = 0, maxpos = b.length, depth = 0, lastFail = -1, reason = "", tracing = true)
  }

  /** Ground rules:
    * - Scip[Boolean] states if something was parsed.
    *   - false results should not have consumed any input
    *   - true results may or may not have consumed input
    *   - these never fail, be careful when using combinators that expect failure.
    * - Scip[Int] indicates how many things have been parsed (for rep and list)
    *   - 0 result has not parsed anything
    * - Scip[A] indicates parsing of some semantic object A
    *   - parsing failure throws a control exception
    *   - anyone catching the exception is responsible of resetting the parsing input to before the failed attempt */
  class Scip[+A](val run0: Scx => A)


  object Scip {
    inline def apply[A](inline run: Scx ?=> A) = new Scip(run(using _))
  }

  inline def scx(using inline scx0: Scx): scx0.type = scx0

  // for reasons beyond me, this seems to help
  inline private def funApply[A, B](inline f: A => B, inline arg: A): B = f(arg)

  extension [A](inline scip: Scip[A]) {
    inline def run(using inline scx: Scx): A = scip.run0(scx)
    // inline def ~[B](inline other: Scip[B]): Scip[Unit]              = Scip { { scip.run; other.run } }
    inline def <~[B](inline other: Scip[B]): Scip[A]       = Scip { { val a = scip.run; other.run; a } }
    inline def ~>[B](inline other: Scip[B]): Scip[B]       = Scip { { scip.run; other.run } }
    inline def <~>[B](inline other: Scip[B]): Scip[(A, B)] = Scip { (scip.run, other.run) }
    inline def |(inline other: Scip[A]): Scip[A] = Scip {
      val start = scx.index
      try scip.run
      catch case e: ScipEx =>
        scx.index = start
        other.run
    }

    inline def opt: Scip[Option[A]] = Scip {
      scip.map(Some.apply).orElse(Option.empty).run
    }
    inline def capture: Scip[(Int, Int)] = Scip {
      val start = scx.index
      scip.run
      val end = scx.index
      (start, end)
    }

    inline def map[B](inline f: A => B): Scip[B]           = Scip { f(scip.run) }
    inline def flatMap[B](inline f: A => Scip[B]): Scip[B] = Scip { funApply(f, scip.run).run }
    inline def withFilter(inline p: A => Boolean): Scip[A] = scip.require(p)

    inline def augment[B](inline f: Scip[A] => Scip[B]): Scip[(A, B)] = Scip{
      var hack: A = null.asInstanceOf
      val b = f(scip.map{x => hack = x; x}).run
      (hack, b)
    }

    inline def length: Scip[Int] = Scip {
      val start = scx.index
      scip.run
      scx.index - start
    }

    inline def orElse[B >: A](inline b: B): Scip[B] = scip | Scip(b)

    /** always backtracks independent of result */
    inline def lookahead: Scip[A] = Scip {
      val start = scx.index
      try scip.run
      finally scx.index = start
    }

    /** converts exceptions into false */
    inline def attempt: Scip[Boolean] = Scip {
      val start = scx.index
      try { scip.run; true }
      catch case e: ScipEx =>
        scx.index = start
        false
    }
    inline def require(inline f: A => Boolean): Scip[A] = Scip {
      val res = scip.run
      if f(res) then res else scx.fail
    }

    inline def list(inline sep: Scip[Boolean]): Scip[List[A]] = Scip {
      val acc         = ListBuffer.empty[A]
      var resultIndex = scx.index
      try
        while
          val start = scx.index
          val res   = scip.run
          resultIndex = scx.index
          acc.addOne(res)
          sep.run && start < scx.index
        do ()
        acc.toList
      catch case e: ScipEx => acc.toList
      finally scx.index = resultIndex
    }

    inline def dropstr: Scip[String] = Scip {
      val start = scx.index
      scip.run
      scx.str(start, scx.index)
    }

    inline def trace(inline name: String): Scip[A] = Scip {
      if !scx.tracing then scip.run
      else
        println(" " * scx.depth * 2 + s"+ $name ${scx.debugat(scx.index)}")
        scx.depth += 1
        try
          val res = scip.run
          scx.depth -= 1
          println(" " * scx.depth * 2 + s"- $name ${scx.debugat(scx.index)} ($res)")
          res
        catch
          case e: ScipEx =>
            scx.depth -= 1
            println(" " * scx.depth * 2 + s"! $name ${scx.debugat(scx.index)}")
            throw e
    }

  }

  extension [A](inline scip: Scip[Scip[A]]) inline def flatten: Scip[A] = Scip(scip.run.run)

  extension (inline scip: Scip[Boolean]) {
    inline def orFail: Scip[Unit] = Scip {
      val start = scx.index
      if scip.run then ()
      else
        scx.index = start
        scx.fail
    }
    inline def str: Scip[String] = scip.orFail.dropstr
    inline def rep: Scip[Int] = Scip {
      var matches = 0
      while scip.run do matches += 1
      matches
    }
    inline def or(inline other: Scip[Boolean]): Scip[Boolean] = Scip { scip.run || other.run }
    inline def and(inline other: Scip[Boolean]): Scip[Boolean] = Scip {
      val start = scx.index
      scip.run && {other.run || {scx.index = start; false}}
    }

    inline def ifso[B](inline other: Scip[B]): Scip[B] = Scip {
      if scip.run then other.run
      else scx.fail
    }

  }

  extension (inline scip: Scip[Int]) {
    inline def min(inline i: Int): Scip[Boolean] = Scip {
      inline constValueOpt[i.type] match
        case Some(0) =>
          scip.run
          true
        case _ =>
          val start = scx.index
          scip.run >= i || { scx.index = start; false }
    }
  }

  inline def scipend: Scip[Boolean] = Scip { scx.index >= scx.maxpos }

  inline def bpred(inline p: Byte => Boolean): Scip[Boolean] = Scip { scx.containsNext(p) }
  inline def cpred(inline p: Int => Boolean): Scip[Boolean] = Scip {
    scx.available(1) && {
      val read = scx.intPred(p)
      scx.index += read
      read > 0
    }
  }

  inline def until(inline end: Scip[Boolean]): Scip[Int] = Scip {
    val start = scx.index
    while !end.lookahead.run && scx.next do ()
    scx.index - start
  }

  extension (s: String) {
    inline def all: Scip[Boolean] = ${ MacroImpls.stringMatchImpl('s) }
    inline def any: Scip[Boolean] = ${ MacroImpls.stringAltImpl('s) }
  }

  inline def seq(b: String): Scip[Boolean]      = seq(b.getBytes(StandardCharsets.UTF_8))
  inline def seq(b: Array[Byte]): Scip[Boolean] = Scip { scx.contains(b) && {scx.index += b.length; true} }
  inline def alt(b: String): Scip[Boolean]      = alt(b.getBytes(StandardCharsets.UTF_8))
  inline def alt(b: Array[Byte]): Scip[Boolean] = Scip {
    scx.available(1) && {
      val cur = scx.peek
      b.exists(_ == cur)
    }
  }

  object MacroImpls {
    def stringMatchImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Boolean]] =
      import quotes.reflect.*
      s.value match
        case None =>
          report.warning(s"value is not constant", s)
          '{ seq($s.getBytes(UTF_8)) }
        case Some(v) => bytesMatchImpl(v.getBytes(UTF_8))

    def bytesMatchImpl(bytes: Array[Byte])(using quotes: Quotes): Expr[Scip[Boolean]] = {
      import quotes.reflect.*
      '{
        Scip { (scx: Scx) ?=>
          scx.available(${ Expr(bytes.length) }) && ${
            val stmts = bytes.iterator.zipWithIndex.map { (b, i) =>
              '{ scx.ahead(${ Expr(i) }, ${ Expr(b) }) }
            }
            stmts.reduceLeft((l, r) => '{ ${ l } && ${ r } })
          } && {
            scx.index += ${ Expr(bytes.length) }
            true
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
          scx.available(1) && ${
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
  }

}
