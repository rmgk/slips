package de.rmgk

import scala.quoted.*

inline def Async[T](inline expr: T): IO[T] =
  ${ asyncImpl('expr) }

def asyncImpl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[IO[T]] =
  import quotes.reflect.*
  println(s"in : ${expr.asTerm.show(using Printer.TreeStructure)}")
  println(s"symbol ${expr.asTerm.symbol}")
  val res = arecimpl(expr)
  println(s"out: ${res.show}")
  res

def arecimpl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[IO[T]] = {
  import quotes.reflect.*

  expr.asTerm match {
    case Inlined(_, _, block @ Block(statements: List[Statement], expr)) =>
      val Block(List(stmt), init) = ValDef.let(Symbol.spliceOwner, "res", expr) { ref =>
        '{ IO { ${ ref.asExprOf[T] } } }.asTerm
      }
      (statements :+ stmt).foldRight[Term](init) { (s, acc) =>
        s match {
          case vd @ ValDef(name, typeTree, Some(Select(io, "await"))) =>
            typeTree.tpe.asType match {
              case '[α] =>
                '{
                  ${ io.asExprOf[IO[α]] }.flatMap[α] { (v: α) =>
                    ${
                      Block(
                        List(ValDef.copy(vd)(name, typeTree, Some('{ v }.asTerm))),
                        acc
                      ).asExprOf[IO[α]]
                    }
                  }
                }.asTerm
            }

          case Select(io, "await") =>
            '{ ${ io.asExprOf[IO[_]] }.flatMap[T] { (_: Any) => ${ acc.asExprOf[IO[T]] } } }.asTerm
          case _ => Block(List(s), acc)
        }
      }.asExprOf[IO[T]]
  }
}
