package de.rmgk

import scala.annotation.compileTimeOnly
import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.{Expr, Quotes, Type}
import scala.util.{Random, Try}

object delay {
  class Sync[-Ctx, +A](val runInContext: Ctx => A) extends Async[Ctx, A](ctx => cb => cb(Right(runInContext(ctx))))

  class SyncCompanion[Ctx]:
    inline def apply[A](inline run: Ctx ?=> A): Sync[Ctx, A] = new Sync(run(using _))
  inline def Sync[Ctx]: SyncCompanion[Ctx] = new SyncCompanion[Ctx] {}

  extension [Ctx, A](inline sync: Sync[Ctx, A]) {
    inline def run(using inline ctx: Ctx): A          = ${ DelayMacros.applyInBlock[Ctx, A]('sync, 'ctx) }
    inline def map[B](inline f: A => B): Sync[Ctx, B] = Sync { f(sync.run) }
    inline def flatMap[B](inline f: A => Sync[Ctx, B]): Sync[Ctx, B] = Sync { f(sync.run).run }
  }

  type CB[-A] = Either[Throwable, A] => Unit

  class Async[-Ctx, +A](val handleInCtx: Ctx => CB[A] => Unit) {
    @compileTimeOnly("await can only be called inside macro")
    def await: A = ???
  }

  extension [Ctx, A](inline async: Async[Ctx, A]) {
    inline def run(inline cb: CB[A])(using inline ctx: Ctx): Unit =
      ${ DelayMacros.handleInBlock[Ctx, A]('async, 'ctx, 'cb) }
    inline def flatMap[B](inline f: A => Async[Ctx, B]): Async[Ctx, B] =
      Async[Ctx].fromCallback[B] { cb =>
        async.run {
          case Right(a) =>
            try delay.run[Ctx, B](f(a))(cb)
            catch case scala.util.control.NonFatal(e) => cb(Left(e))
          case Left(err) => cb(Left(err))
        }
      }
  }

  trait AsyncCompanion[Ctx] {
    inline def fromCallback[A](inline f: Ctx ?=> CB[A] => Unit): Async[Ctx, A] =
      new Async(ctx => cb => f(using ctx)(cb))

    inline def fromFuture[A](inline fut: Future[A])(using inline ec: ExecutionContext): Async[Ctx, A] =
      Async.fromCallback { cb =>
        fut.onComplete(t => cb(t.toEither))
      }

    inline def apply[A](inline expr: Ctx ?=> A): Async[Ctx, A] =
      new Async[Ctx, A](ctx => cb => syntax(expr(using ctx)).run(cb)(using ctx))

    inline def syntax[A](inline expr: A): Async[Ctx, A] =
      ${ DelayMacros.asyncImpl[Ctx, A]('{ expr }) }
  }
  inline def Async[Ctx]: AsyncCompanion[Ctx] = new AsyncCompanion[Ctx] {}

  extension [A](inline fut: Future[A])(using ExecutionContext) inline def await: A = Async.fromFuture(fut).await

  object DelayMacros {
    def applyInBlock[Ctx: Type, B: Type](
        dio: Expr[Sync[Ctx, B]],
        ctx: Expr[Ctx]
    )(using quotes: Quotes): Expr[B] = {
      import quotes.reflect.*
      val maybeBlock = cleanBlock(dio.asTerm)
      val fixed = maybeBlock match {
        case Block(stmts, expr) => expr.asExpr match {
            case '{ new Sync[α, B]($scxfun) } =>
              if !(TypeRepr.of[Ctx] <:< TypeRepr.of[α]) then
                report.errorAndAbort(s"»${Type.show[Ctx]}« is not a subtype of »${Type.show[α]}«", expr.asExpr)
              Some(
                cleanBlock(Block(stmts, Expr.betaReduce('{ $scxfun.apply($ctx.asInstanceOf[α]) }).asTerm)).asExprOf[B]
              )
            case other => None
          }
        case other => None
      }
      Expr.betaReduce(fixed.getOrElse('{ $dio.runInContext($ctx) }))
    }

    def handleInBlock[Ctx: Type, B: Type](
        dio: Expr[Async[Ctx, B]],
        ctx: Expr[Ctx],
        cb: Expr[CB[B]]
    )(using quotes: Quotes): Expr[Unit] = {
      import quotes.reflect.*
      val maybeBlock = cleanBlock(dio.asTerm)
      val fixed = maybeBlock match {
        case Block(stmts, expr) => expr.asExpr match {
            case '{ new Async[Ctx, B]($scxfun) } =>
              Some(cleanBlock(Block(stmts, Expr.betaReduce('{ $scxfun.apply($ctx).apply($cb) }).asTerm)).asExprOf[Unit])
            case '{ new Sync[Ctx, B]($scxfun) } =>
              Some(
                cleanBlock(Block(
                  stmts,
                  Expr.betaReduce('{ $cb(Right($scxfun($ctx))) }).asTerm
                )).asExprOf[Unit]
              )
            case other => None
          }
        case other => None
      }
      Expr.betaReduce(fixed.getOrElse('{ $dio.handleInCtx($ctx)($cb) }))
    }

    def cleanBlock[A](using quotes: Quotes)(expr: quotes.reflect.Term): quotes.reflect.Term = {
      import quotes.reflect.*
      expr match
        case Inlined(_, _, t)                                 => cleanBlock(t)
        case Match(_, List(CaseDef(Wildcard(), None, inner))) => cleanBlock(inner)
        case Block(statements, expr) => cleanBlock(expr) match
            case Block(innerstmts, expr) => Block(statements ::: innerstmts, expr)
            case expr                    => Block(statements, expr)
        case Typed(expr, tt)   => cleanBlock(expr)
        case NamedArg(_, expr) => cleanBlock(expr)
        case other             => Block(Nil, other)
    }

    object CleanBlock {
      def unapply(using quotes: Quotes)(expr: quotes.reflect.Term): Some[quotes.reflect.Term] = {
        Some(cleanBlock(expr))
      }
    }

    def asyncImpl[Ctx: Type, T: Type](expr: Expr[T])(using quotes: Quotes): Expr[Async[Ctx, T]] = {
      import quotes.reflect.*

      cleanBlock(expr.asTerm) match {
        case block @ Block(statements: List[Statement], expr) =>
          val Block(List(stmt), init) = ValDef.let(Symbol.spliceOwner, "async$macro$result", expr) { ref =>
            '{ Sync[Ctx][T] { ${ ref.asExprOf[T] } } }.asTerm
          }
          (statements :+ stmt).foldRight[Term](init) { (s, acc) =>
            s match {
              case vd @ ValDef(name, typeTree, Some(CleanBlock(Block(stmts, Select(io, "await"))))) =>
                typeTree.tpe.asType match {
                  case '[α] =>
                    '{
                      ${ (if stmts.isEmpty then io else Block(stmts, io)).asExprOf[Async[Ctx, α]] }.flatMap[T] {
                        (v: α) =>
                          ${
                            Block(
                              List(ValDef.copy(vd)(name, typeTree, Some('{ v }.asTerm))),
                              acc
                            ).asExprOf[Async[Ctx, T]]
                          }
                      }
                    }.asTerm
                }
              case CleanBlock(Block(stmts, Select(io, "await"))) =>
                '{
                  ${ (if stmts.isEmpty then io else Block(stmts, io)).asExprOf[Async[Ctx, Any]] }.flatMap[T] {
                    (_: Any) =>
                      ${ acc.asExprOf[Async[Ctx, T]] }
                  }
                }.asTerm
              case _ =>
                Block(List(s), acc)
            }
          }.asExprOf[Async[Ctx, T]]
        case other =>
          '{ Sync[Ctx][T]($expr) }
      }
    }
  }

}
