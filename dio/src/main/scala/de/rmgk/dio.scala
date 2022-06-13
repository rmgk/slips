package de.rmgk

import scala.quoted.{Expr, Quotes, Type}

object dio {
  class Dio[Ctx, +A](val runInContext: Ctx => A)

  object Dio {
    inline def apply[Ctx, A](inline run: Ctx ?=> A): Dio[Ctx, A] = new Dio(run(using _))
  }

  extension [Ctx, A](inline dio: Dio[Ctx, A]) {
    inline def run(using inline ctx: Ctx): A = ${ MacroImpls.applyInBlock[Ctx, A]('dio, 'ctx) }
    inline def map[B](inline f: A => B): Dio[Ctx, B]           = Dio { f(dio.run) }
    inline def flatMap[B](inline f: A => Dio[Ctx, B]): Dio[Ctx, B] = Dio { f(dio.run).run }
  }

  object MacroImpls {
    def applyInBlock[Ctx: Type, B: Type](dio: Expr[Dio[Ctx, B]], ctx: Expr[Ctx])(using quotes: Quotes): Expr[B] = {
      import quotes.reflect.*
      val maybeBlock = cleanBlock(dio.asTerm)
      val fixed = maybeBlock match {
        case Block(stmts, expr) => expr.asExpr match {
            case '{ new Dio[Ctx, B]($scxfun) } =>
              Some(cleanBlock(Block(stmts, Expr.betaReduce('{ $scxfun.apply($ctx) }).asTerm)).asExprOf[B])
            case other => None
          }
        case other => None
      }
      Expr.betaReduce(fixed.getOrElse('{ $dio.runInContext($ctx) }))
    }

    def cleanBlock[A](using quotes: Quotes)(expr: quotes.reflect.Term): quotes.reflect.Term = {
      import quotes.reflect.*
      expr match
        case Inlined(_, _, t)                                 => cleanBlock(t)
        case Match(_, List(CaseDef(Wildcard(), None, inner))) => cleanBlock(inner)
        case Block(statements, expr) => cleanBlock(expr) match
            case Block(innerstmts, expr) => Block(statements ::: innerstmts, expr)
            case expr                    => Block(statements, expr)
        case Typed(expr, tt) => cleanBlock(expr)
        case other           => other
    }
  }
}
