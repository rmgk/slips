package de.rmgk

import scala.quoted.*

inline def dio[T](inline expr: T): IO[T] =
  ${ impl('expr) }

def impl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[IO[T]] =
  import quotes.reflect.*
  println(s"in : ${expr.asTerm.show(using Printer.TreeStructure)}")
  println(s"symbol ${expr.asTerm.symbol}")
  val res = recimpl(expr)
  println(s"out: ${res.show}")
  res

def recimpl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[IO[T]] =
  import quotes.reflect.*

  expr match
    case '{ { $a: IO[α]; $rest } } =>
      val rec = recimpl(rest)
      '{ $a.flatMap { _ => $rec } }.asExprOf[IO[T]]

    case '{ { val awt = (($a: IO[α]).await); $rest(awt): T } } =>
      println(s"rest: ${rest.show}")
      println(s"rest: ${rest.asTerm}")
      '{
        $a.flatMap[T] { (y: α) =>
          ${
            val reduced = Expr.betaReduce('{ ${ rest }(y) })
            println(s"reduced: ${reduced.show}")
            recimpl(reduced)
          }
        }
      }

    case '{ { val vd = $a: α; $rest(vd): T } } =>
      '{ val y = $a; ${ recimpl(Expr.betaReduce('{ ${ rest }(y) })) } }

    case '{ { $expr } } =>
      '{ IO($expr) }
