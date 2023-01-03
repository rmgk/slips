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
import scala.deriving.Mirror
import scala.compiletime.summonFrom
import scala.compiletime.constValueOpt

object scip {

  trait ScipEx extends ControlThrowable

  extension (inline s: String) private inline def getU8Bytes: Array[Byte] =
    import scala.language.unsafeNulls
    s.getBytes(StandardCharsets.UTF_8)

  case class Scx(
      val input: Array[Byte],
      var index: Int,
      val maxpos: Int,
      var depth: Int,
      var lastFail: Int,
      val tracing: Boolean,
  ) {

    object ScipExInstance extends ScipEx {
      override def getMessage: String = s"idx: ${debugat(index)}; fail: ${debugat(lastFail)}"
    }

    def debugat(i: Int): String = s"${index}»${str(i, i + 12).replaceAll("\\n", "\\\\n")}«"

    def str(l: Int, r: Int) = new String(input, l, math.min(r, maxpos) - l, UTF_8)

    def contains(bytes: Array[Byte]): Boolean =
      val len = bytes.length
      available(len) || { return false }
      @tailrec def rec(i: Int): Boolean = i >= len || bytes(i) == input(index + i) && rec(i + 1); rec(0)
      // java.util.Arrays.equals(bytes, 0, bytes.length, input, index, math.min(index + bytes.length, maxpos))

    def available(min: Int): Boolean    = maxpos >= index + min
    def ahead(i: Int, b: Byte): Boolean = input(index + i) == b

    inline def intPred(inline p: Int => Boolean): Int = {
      val bs = this.peek
      val b  = bs & 0xff
      if (b & Utf8bits.maxBit) == 0 then if p(b) then 1 else 0
      else
        val count = Integer.numberOfLeadingZeros(~bs) - 24
        val v = (count: @switch) match
          case 2 => parse2(b)
          case 3 => parse3(b)
          case 4 => parse4(b)
        if (p(v)) count else 0
    }

    private def parse2(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 5), 1, 2)
    private def parse3(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 4), 1, 3)
    private def parse4(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 3), 1, 4)

    object Utf8bits {

      inline val maxBit: 128                               = 1 << 7
      inline def lowest(inline b: Int, inline n: Int): Int = b & ((1 << n) - 1)
      inline def addLowest(b: Int, acc: Int, n: Int): Int  = lowest(b, n) | (acc << n)
      inline def grab(acc: Int, inline pos: Int, inline max: Int): Int =
        inline if (pos >= max) then acc
        else grab(addLowest(input(index + pos) & 0xff, acc, 6), pos + 1, max)
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
      import scala.language.unsafeNulls
      val b = s.getBytes(UTF_8)
      Scx(b, index = 0, maxpos = b.length, depth = 0, lastFail = -1, tracing = true)
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
    *   - anyone catching the exception is responsible of resetting the parsing input to before the failed attempt
    */
  type Scip[+A] = de.rmgk.delay.Sync[Scx, A]

  object Scip {
    inline def apply[A](inline run: Scx ?=> A): Scip[A] = new de.rmgk.delay.Sync(run(using _))
  }

  inline def scx(using inline scx0: Scx): scx0.type = scx0

  extension [A](inline scip: Scip[A]) {
    inline def <~[B](inline other: Scip[B]): Scip[A]       = Scip { val a = scip.run; other.run; a }
    inline def ~>[B](inline other: Scip[B]): Scip[B]       = Scip { scip.run; other.run }
    inline def <~>[B](inline other: Scip[B]): Scip[(A, B)] = Scip { (scip.run, other.run) }
    inline def |[B >: A](inline other: Scip[B]): Scip[B] = Scip {
      val start = scx.index
      try scip.run
      catch
        case e: ScipEx =>
          scx.index = start
          other.run
    }
    inline def opt: Scip[Option[A]] = scip.map(Some.apply) | Scip { None }

    inline def region: Scip[(Int, Int)] = Scip {
      val start = scx.index
      scip.run
      (start, scx.index)
    }

    inline def byteCount: Scip[Int] = Scip {
      val start = scx.index
      scip.run
      scx.index - start
    }

    inline def augment[B](inline f: Scip[A] => Scip[B]): Scip[(A, B)] = Scip {
      var hack: A = null.asInstanceOf
      val b = f(scip.map { x =>
        hack = x; x
      }).run
      if (hack == null) scx.fail
      (hack, b)
    }

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
      catch
        case e: ScipEx =>
          scx.index = start
          false
    }
    inline def withFilter(inline p: A => Boolean): Scip[A] = scip.require(p)
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
          println(" " * scx.depth * 2 + s"- $name ${scx.debugat(scx.index)} (${s"$res".take(42)})")
          res
        catch
          case e: ScipEx =>
            scx.depth -= 1
            println(" " * scx.depth * 2 + s"! $name ${scx.debugat(scx.lastFail)}")
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
    inline def opt: Scip[true]                                = Scip { scip.run; true }
    inline def or(inline other: Scip[Boolean]): Scip[Boolean] = Scip { scip.run || other.run }
    inline def and(inline other: Scip[Boolean]): Scip[Boolean] = Scip {
      val start = scx.index
      scip.run && { other.run || { scx.index = start; false } }
    }

    inline def ifso[B](inline other: Scip[B]): Scip[B] = Scip {
      if scip.run then other.run
      else scx.fail
    }

  }

  extension (inline scip: Scip[Int]) {
    inline def min(i: Int): Scip[Boolean] = Scip {
      inline i match
        case 0 =>
          scip.run
          true
        case _ =>
          val start = scx.index
          scip.run >= i || { scx.index = start; false }
    }
  }

  inline def end: Scip[Boolean] = Scip { scx.index >= scx.maxpos }

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

  inline def seq(inline b: String): Scip[Boolean] =
    val bytes = b.getU8Bytes
    seq(bytes)
  inline def seq(b: Array[Byte]): Scip[Boolean] = Scip { scx.contains(b) && { scx.index += b.length; true } }
  inline def alt(inline b: String): Scip[Boolean] =
    val bytes = b.getU8Bytes
    alt(bytes)
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
          '{ seq($s.getU8Bytes) }
        case Some(v) => bytesMatchImpl(v.getU8Bytes)

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
        case Some(v) =>
          val bytes = v.map(_.toString.getU8Bytes.toSeq)
          bytesAltImpl(trieFromBytes(bytes))

    case class MiniTrie(elems: Seq[Byte], children: Map[Byte, MiniTrie])

    def trieFromBytes(bytes: Seq[Seq[Byte]]): MiniTrie = {
      val elems    = bytes.filter(_.length == 1).map(_.head)
      val other    = bytes.filter(_.length > 1)
      val children = other.groupBy(_.head).view.mapValues(ss => trieFromBytes(ss.map(_.drop(1)))).toMap
      MiniTrie(elems, children)
    }

    def bytesAltImpl(outerBytes: MiniTrie)(using quotes: Quotes): Expr[Scip[Boolean]] = {
      import quotes.reflect.*
      '{
        Scip { (scx: Scx) ?=>
          ${
            def terminals(bytes: Seq[Byte], len: Int): Option[CaseDef] =
              if bytes.isEmpty then None
              else
                Some(CaseDef(
                  Alternatives(bytes.sorted.distinct.toList.map { b => Expr(b).asTerm }),
                  None,
                  '{
                    scx.index += ${ Expr(len) }
                    true
                  }.asTerm
                ))

            val wildcardCase = CaseDef(Wildcard(), None, '{ false }.asTerm)

            def recurse(bytes: MiniTrie, level: Int): Expr[Boolean] = {
              val termCases = terminals(bytes.elems, level)

              val childCases = bytes.children.map { (b, trie) =>
                val inner = recurse(trie, level + 1)
                CaseDef(Expr(b).asTerm, None, inner.asTerm)
              }

              val res = '{
                scx.available(${ Expr(level) }) && ${
                  Match(
                    '{ scx.input(scx.index + ${ Expr(level - 1) }) }.asTerm,
                    List[IterableOnce[CaseDef]](
                      termCases,
                      childCases,
                      Some(wildcardCase),
                    ).flatten
                  ).asExprOf[Boolean]
                }
              }
              res
            }
            recurse(outerBytes, 1)
          }
        }
      }
    }
  }

}
