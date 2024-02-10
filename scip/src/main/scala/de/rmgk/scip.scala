package de.rmgk

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.{switch, tailrec}
import scala.collection.mutable.ListBuffer
import scala.quoted.*
import scala.util.control.ControlThrowable
import scala.compiletime.summonFrom
import scala.compiletime.constValueOpt

object scip {

  trait ScipEx extends ControlThrowable

  case class Scx(
      /** Every operator that works on strings assumes this to be utf-8 */
      val input: Array[Byte],
      var index: Int,

      /** Marks the end of the input range to be consider. Usually equal to `input.length` */
      val maxpos: Int,

      /** enable output for the `trace` combinator */
      val tracing: Boolean,

      /** tracks nesting level for tracing */
      var depth: Int,
      /** Used for the exception below.
        * Note, once set, this is never reset (even if the failure is backtracked and handled), so this is not an indication of a parse failure.
        */
      var lastFail: Int,
  ) {

    /** Used for allocation free signalling of parse errors. */
    object ScipExInstance extends ScipEx {
      override def getMessage: String =
        s"idx: ${debugat(index)}${
            if lastFail > 0
            then s"; fail: ${debugat(lastFail)}"
            else ""
          }"
    }

    def debugat(i: Int): String = s"${index}»${str(i, i + 12).replaceAll("\\n", "\\\\n")}«"

    def str(l: Int, r: Int) = new String(input, l, math.min(r, maxpos) - l, UTF_8)

    def contains(bytes: Array[Byte]): Boolean =
      val len                           = bytes.length
      @tailrec def rec(i: Int): Boolean = i >= len && index + i <= maxpos || bytes(i) == input(index + i) && rec(i + 1)
      available(len) && rec(0)
      // Not available in Scala native (and also kinda longer, wtf)
      // java.util.Arrays.equals(bytes, 0, bytes.length, input, index, math.min(index + bytes.length, maxpos))

    def available(min: Int): Boolean    = maxpos >= index + min
    def ahead(i: Int, b: Byte): Boolean = input(index + i) == b

    inline def intPred(inline p: Int => Boolean): Int = {
      val bs = this.peek
      val b  = bs & 0xff
      if (b & Utf8bits.maxBit) == 0 then if p(b) then 1 else 0
      else Utf8bits.utf8Pred(p, b)
    }

    object Utf8bits {

      def utf8Pred(p: Int => Boolean, b: Int) = {
        val count = Integer.numberOfLeadingZeros(~(b << 24))
        val v = (count: @switch) match
          case 2 => parse2(b)
          case 3 => parse3(b)
          case 4 => parse4(b)
          case other =>
            throw IllegalStateException(
              s"byte ${b.toBinaryString} at ${index} is not a legal utf-8 value.",
              ScipExInstance
            )
        if (p(v)) count
        else 0
      }

      inline def parse2(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 5), 1, 2)
      inline def parse3(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 4), 1, 3)
      inline def parse4(b: Int): Int = Utf8bits.grab(Utf8bits.addLowest(b, 0, 3), 1, 4)

      inline val maxBit: 128                               = 1 << 7
      inline def lowest(inline b: Int, inline n: Int): Int = b & ((1 << n) - 1)
      inline def addLowest(b: Int, acc: Int, n: Int): Int  = lowest(b, n) | (acc << n)
      inline def grab(acc: Int, inline pos: Int, inline max: Int): Int =
        inline if pos >= max then acc
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
      val b = s.getU8Bytes
      new Scx(b, index = 0, maxpos = b.length, depth = 0, lastFail = -1, tracing = true)
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

  transparent inline def scx(using inline scx0: Scx): Scx = scx0

  extension [A](inline scip: Scip[A]) {
    inline def <~[B](inline other: Scip[B]): Scip[A]       = Scip { val a = scip.run; other.run; a }
    inline def ~>[B](inline other: Scip[B]): Scip[B]       = Scip { scip.run; other.run }
    inline def <~>[B](inline other: Scip[B]): Scip[(A, B)] = Scip { (scip.run, other.run) }
    inline def |[B >: A](inline other: Scip[B]): Scip[B] = Scip {
      val `start|` = scx.index
      try scip.run
      catch
        case e: ScipEx =>
          scx.index = `start|`
          other.run
    }
    inline def opt: Scip[Option[A]] = scip.map(Some.apply) | Scip { None }

    inline def region: Scip[(Int, Int)] = Scip {
      val startRegion = scx.index
      scip.run
      (startRegion, scx.index)
    }

    inline def byteCount: Scip[Int] = Scip {
      val startBytes = scx.index
      scip.run
      scx.index - startBytes
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
      val startLookahead = scx.index
      try scip.run
      finally scx.index = startLookahead
    }

    /** converts exceptions into false */
    inline def attempt: Scip[Boolean] = Scip {
      val startAttempt = scx.index
      try { scip.run; true }
      catch
        case e: ScipEx =>
          scx.index = startAttempt
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
          val startList = scx.index
          val res       = scip.run
          resultIndex = scx.index
          acc.addOne(res)
          sep.run && startList < scx.index
        do ()
        acc.toList
      catch case e: ScipEx => acc.toList
      finally scx.index = resultIndex
    }

    inline def dropstr: Scip[String] = Scip {
      val startStr = scx.index
      scip.run
      scx.str(startStr, scx.index)
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
      val startOrFail = scx.index
      if scip.run then ()
      else
        scx.index = startOrFail
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
      val startAnd = scx.index
      scip.run && { other.run || { scx.index = startAnd; false } }
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
          val startMin = scx.index
          scip.run >= i || { scx.index = startMin; false }
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
    val startUntil = scx.index
    while !end.lookahead.run && scx.next do ()
    scx.index - startUntil
  }

  extension (s: String) {
    inline def all: Scip[Boolean] = ${ MacroImpls.stringMatchImpl('{ s }) }
    inline def any: Scip[Boolean] = ${ MacroImpls.stringAltImpl('{ s }) }
  }

  inline def choice(inline ss: String*): Scip[Boolean] =
    ${ MacroImpls.stringChoice('{ ss }) }

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

  extension (inline s: String)
    private inline def getU8Bytes: Array[Byte] =
      import scala.language.unsafeNulls
      s.getBytes(StandardCharsets.UTF_8)

  object MacroImpls {
    def stringMatchImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Boolean]] =
      import quotes.reflect.*
      s.value match
        case None =>
          report.warning(s"value is not constant", s)
          '{ seq($s.getU8Bytes) }
        case Some(v) =>
          val bytes = v.getU8Bytes
          '{
            Scip {
              ${ bytesMatchImpl(bytes, 0) }.run && {
                scx.index += ${ Expr(bytes.length) }
                true
              }
            }
          }

    def bytesMatchImpl(bytes: Array[Byte], offset: Int)(using quotes: Quotes): Expr[Scip[Boolean]] = {
      '{
        Scip { (scx: Scx) ?=>
          scx.available(${ Expr(bytes.length + offset) }) && ${
            val stmts = bytes.iterator.zipWithIndex.map { (b, i) =>
              '{ scx.ahead(${ Expr(i + offset) }, ${ Expr(b) }) }
            }
            stmts.reduceLeft((l, r) => '{ ${ l } && ${ r } })
          }
        }
      }
    }

    def stringChoice(s: Expr[Seq[String]])(using quotes: Quotes): Expr[Scip[Boolean]] =
      import quotes.reflect.*
      s.value match
        case None =>
          report.errorAndAbort(s"parameters are not constant", s)
        case Some(v) =>
          val bytes = v.map(_.getU8Bytes.toSeq)
          val trie  = trieFromBytes(bytes)
          bytesAltImpl(trie)

    def stringAltImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Boolean]] =
      import quotes.reflect.*
      s.value match
        case None =>
          report.errorAndAbort(s"value is not constant", s)
        case Some(v) =>
          val bytes = v.map(_.toString.getU8Bytes.toSeq)
          bytesAltImpl(trieFromBytes(bytes))

    /** [[terminals]] are terminal bytes where the match ends */
    case class MiniTrie(terminals: Set[Byte], children: Map[Set[Byte], MiniTrie]) {
      def isEmpty = terminals.isEmpty && children.isEmpty
    }

    def trieFromBytes(bytes: Seq[Seq[Byte]]): MiniTrie = {
      val elems = bytes.filter(_.length == 1).map(_.head).toSet
      val other = bytes.filter(_.length > 1)
      val tries = other.groupBy(_.head).view.mapValues(ss => trieFromBytes(ss.map(_.drop(1))))
      val children = tries.groupBy(_._2).map { (trie, orig) =>
        val bytes = orig.map(_._1).toSet
        bytes -> trie
      }.toMap
      MiniTrie(elems, children)
    }

    def bytesAltImpl(outerBytes: MiniTrie)(using quotes: Quotes): Expr[Scip[Boolean]] = {
      import quotes.reflect.*
      '{
        Scip { (scx: Scx) ?=>
          ${

            def makeCase(bytes: Set[Byte], inner: Term) = CaseDef(
              Alternatives(bytes.toList.sorted.map { b => Expr(b).asTerm }),
              None,
              inner
            )

            def terminals(bytes: Set[Byte], offset: Int): Option[CaseDef] =
              if bytes.isEmpty then None
              else
                Some(
                  makeCase(
                    bytes,
                    '{
                      // only the terminal case increases the index, so we increase by the full length of the matched sequence
                      scx.index += ${ Expr(offset + 1) }
                      true
                    }.asTerm
                  )
                )

            val wildcardCase = CaseDef(Wildcard(), None, '{ false }.asTerm)

            def recurse(bytes: MiniTrie, offset: Int): Expr[Boolean] = {

              def grabPrefix(bytes: MiniTrie, acc: Seq[Byte]): (Seq[Byte], MiniTrie) =
                if bytes.children.isEmpty && bytes.terminals.sizeIs == 1
                then (acc :+ bytes.terminals.head, MiniTrie.apply(Set.empty, Map.empty))
                else if bytes.terminals.isEmpty && bytes.children.sizeIs == 1 && bytes.children.head._1.sizeIs == 1
                then
                  val (byteSet, trie) = bytes.children.head
                  grabPrefix(trie, acc :+ byteSet.head)
                else
                  (acc, bytes)

              val (prefix, rest) = grabPrefix(bytes, Seq.empty)
              if prefix.nonEmpty
              then
                val array = prefix.toArray
                if rest.isEmpty
                then
                  '{
                    ${ bytesMatchImpl(array, offset) }.run && {
                      scx.index += ${ Expr(offset + array.length) }
                      true
                    }
                  }
                else
                  val inner = recurse(rest, offset + array.size)
                  '{
                    ${ bytesMatchImpl(array, offset) }.run && $inner
                  }
              else
                val bytes = rest

                val termCases = terminals(bytes.terminals, offset)

                val childCases = bytes.children.map { (bytes, trie) =>
                  val inner = recurse(trie, offset + 1)
                  makeCase(bytes, inner.asTerm)
                }

                '{
                  scx.available(${ Expr(offset + 1) }) && ${
                    Match(
                      '{ scx.input(scx.index + ${ Expr(offset) }) }.asTerm,
                      List[IterableOnce[CaseDef]](
                        termCases,
                        childCases,
                        Some(wildcardCase),
                      ).flatten
                    ).asExprOf[Boolean]
                  }
                }
            }
            recurse(outerBytes, 0)
          }
        }
      }
    }
  }
}
