package de.rmgk

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.compiletime.erasedValue
import scala.quoted.*
import scala.util.control.ControlThrowable

object scip {

  class ScipEx(message: => String) extends ControlThrowable {
    override def getMessage: String = message
  }

  case class Scx(
      val input: Array[Byte],
      var index: Int,
      var goodIndex: Int,
  ) {
    def fail(msg: => String): Nothing =
      throw new ScipEx(s"$msg\nat: »${input.view.slice(index, index + 42).str}«")

    def ahead(i: Int): Byte                 = input(index + i)
    def slice(i: Int): IndexedSeqView[Byte] = input.view.slice(index, index + 1)
    def assertAvailable(len: Int): Unit =
      val rem = input.length - index - len
      if rem < 0 then fail(s"end of input reached, but ${-rem} more bytes requested")

    val EOT: Byte = 3.toByte

    def readSafe: Byte    = if index < input.length then input(index) else EOT
    inline def peek: Byte = input(index)
    def next: Boolean =
      index += 1
      index < input.length

    inline def containsNext(inline p: Byte => Boolean): Boolean =
      index < input.length && p(peek) && { index += 1; true }
    def assertNext(b: Byte): Unit =
      input(index) == b || fail(s"expected $b")
      index += 1
  }
  object Scx {
    def apply(s: String): Scx = Scx(s.getBytes(UTF_8), 0, 0)
  }

  class Scip[+A](val run0: Scx => A)

  object Scip {
    inline def apply[A](inline run: Scx ?=> A) = new Scip(run(using _))
  }

  inline def scx(using inline scx0: Scx): scx0.type = scx0

  extension [A](inline scip: Scip[A]) {
    inline def run(using inline scx: Scx): A = scip.run0(scx)
    inline def ~[B](inline other: Scip[B]): Scip[Unit] = Scip { scx ?=>
      scip.run
      other.run
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
    inline def runOrElse(inline b: A): Scip[A] = Scip {
      try scip.run
      catch case e: ScipEx => b
    }
    inline def lookahead: Scip[A] = Scip {
      val start = scx.index
      try scip.run
      finally scx.index = start
    }
    inline def test: Scip[Boolean] = Scip {
      try { scip.run; true }
      catch case e: ScipEx => false
    }
  }

  extension (s: String) inline def scip: Scip[Unit] = ${ exactImpl('s) }
  def exactImpl(s: Expr[String])(using quotes: Quotes): Expr[Scip[Unit]] =
    import quotes.reflect.*
    s.value match
      case None => '{ exact($s.getBytes(UTF_8)) }
      case Some(v) =>
        val bytes = v.getBytes(UTF_8).toList
        '{
          Scip { (scx: Scx) ?=>
            ${
              val stmts: List[Statement] = bytes.map(b => '{ scx.assertNext(${ Expr(b) }) }.asTerm)
              val (start, last)          = stmts.splitAt(stmts.length - 1)
              (if start.isEmpty then last.head
               else Block(start, last.head.asInstanceOf[Term])).asExprOf[Unit]
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

  inline def characters: Scip[Unit] = Scip {
    while scx.containsNext(byte => ('a' <= byte && byte <= 'z') || ('A' <= byte && byte <= 'Z')) do ()
  }

  inline def whitespace: Scip[Unit] = Scip { while Character.isWhitespace(scx.readSafe) do scx.index += 1 }

  inline def choice[T](inline alternatives: Scip[T]*): Scip[T] = ${ choiceImpl('alternatives) }
  // Scip { scx ?=>
  //  @tailrec
  //  def rec(l: List[Scip[T]]): T = l match
  //    case Nil               => scx.fail(s"no alternative matches")
  //    case (h: Scip[T]) :: t => h.runOrElse { rec(t) }.run
  //  rec(alternatives.toList)
  // }

  def choiceImpl[T: Type](alternatives: Expr[Seq[Scip[T]]])(using quotes: Quotes): Expr[Scip[T]] = {
    import quotes.reflect.*
    alternatives match
      case Varargs(args) => '{
          Scip { scx ?=>
            ${
              args.foldRight[Expr[T]]('{ scx.fail("no alternative matched") }) { (next: Expr[Scip[T]], acc) =>
                '{
                  ($next.runOrElse(null.asInstanceOf[T]).run(using scx): T) match
                    case null   => $acc
                    case res: T => res
                }
              }
            }
          }
        }
  }

  inline def until(inline end: Scip[Any]): Scip[Unit] = Scip {
    while !end.test.run && scx.next do ()
  }

  extension (isv: IndexedSeqView[Byte]) {
    def str: String = new String(isv.toArray, UTF_8)
  }
  extension (inline isv: Scip[IndexedSeqView[Byte]]) {
    inline def str: Scip[String] = isv.map(_.str)
  }

  inline def unitOpt[T, A](inline normal: A): A =
    inline erasedValue[T] match
      case _: Unit => null.asInstanceOf[A]
      case other   => normal

  inline def repeat[A](inline it: Scip[A], inline sep: Scip[Any]): Scip[List[A]] = Scip {
    var continue = true
    val acc      = unitOpt[A, ListBuffer[A]](ListBuffer.empty[A])
    while continue do
      val res = it.parseNonEmpty.accept.run
      unitOpt[A, ListBuffer[A]](acc.addOne(res))
      try sep.accept.run
      catch case e: ScipEx => continue = false
    val res = unitOpt[A, List[A]](acc.toList)
    if res == null then Nil else res
  }

}
