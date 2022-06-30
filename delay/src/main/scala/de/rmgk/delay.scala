package de.rmgk

import java.util.Objects
import java.util.Objects.isNull
import java.util.concurrent.CompletionStage
import scala.annotation.compileTimeOnly
import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.{Expr, Quotes, Type}
import scala.util.{Random, Try}
import scala.util.control.NonFatal

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

  type Callback[-A] = Either[Throwable, A] => Unit
  extension [A](inline handler: Callback[A]) {
    inline def complete(tr: Try[A]): Unit               = handler(tr.toEither)
    inline def complete(tr: Either[Throwable, A]): Unit = handler(tr)
    inline def succeed(value: A): Unit                  = handler(Right(value))
    inline def fail(ex: Throwable): Unit                = handler(Left(ex))
  }

  class Async[-Ctx, +A](val handleInCtx: Ctx => Callback[A] => Unit) {
    @compileTimeOnly("await must be used inside Async and may not be nested inside of expressions")
    def await: A = ???
  }

  extension [Ctx, A](inline async: Async[Ctx, A]) {
    inline def run(inline cb: Callback[A])(using inline ctx: Ctx): Unit =
      ${ DelayMacros.handleInBlock[Ctx, A]('async, 'ctx, 'cb) }
    inline def map[B](inline f: A => B): Async[Ctx, B] =
      Async {
        val value = async.await
        f(value)
      }
    inline def flatMap[B](inline f: A => Async[Ctx, B]): Async[Ctx, B] =
      Async.fromCallback {
        async.run {
          case Right(a) =>
            try delay.run(f(a))(Async.handler)
            catch case scala.util.control.NonFatal(e) => Async.handler.fail(e)
          case Left(err) => Async.handler.fail(err)
        }
      }
    inline def runToFuture(using inline ctx: Ctx): Future[A] =
      val p = scala.concurrent.Promise[A]()
      async.run {
        case Left(e)  => p.failure(e)
        case Right(v) => p.success(v)
      }
      p.future

    inline def runToAsync(using inline ctx: Ctx): Async[Ctx, A] =
      val p = new Promise[A]
      async.run(p)
      p.async

    inline def close(inline handler: Ctx ?=> Unit): Async[Ctx, A] =
      new Async[Ctx, A](ctx =>
        cb =>
          async.run { res =>
            handler(using ctx)
            cb(res)
          }(using ctx)
      )
  }

  trait AsyncCompanion[Ctx] {
    inline def fromCallback[A](inline f: Ctx ?=> Callback[A] ?=> Unit): Async[Ctx, A] =
      new Async(ctx => cb => f(using ctx)(using cb))

    inline def handler[A](using cb: Callback[A]): Callback[A] = cb

    inline def apply[A](inline expr: Ctx ?=> A): Async[Ctx, A] =
      new Async[Ctx, A](ctx =>
        cb => {
          try syntax(expr(using ctx)).run(cb)(using ctx)
          catch case NonFatal(e) => cb(Left(e))
        }
      )

    inline def syntax[A](inline expr: A): Async[Ctx, A] =
      ${ DelayMacros.asyncImpl[Ctx, A]('{ expr }) }
  }
  inline def Async[Ctx]: AsyncCompanion[Ctx] = new AsyncCompanion[Ctx] {}

  extension [A](inline fut: Future[A]) {
    inline def toAsync(using inline ec: ExecutionContext): Async[Any, A] =
      Async.fromCallback {
        fut.onComplete(Async.handler.complete(_))
      }
  }

  extension [Ctx, T](inline cs: Ctx ?=> CompletionStage[T]) {
    inline def toAsync: Async[Ctx, T] =
      Async.fromCallback {
        cs.handle { (res, ex) =>
          if !isNull(ex) then Async.handler.fail(ex)
          else if !isNull(res) then Async.handler.succeed(res)
          else Async.handler.fail(IllegalStateException("completion stage returned nothing without failure"))
        }
      }
  }

  class Promise[T] extends Callback[T] {
    @volatile private var value: Option[Either[Throwable, T]] = None
    @volatile private var callbacks: List[Callback[T]]        = Nil

    override def apply(res: Either[Throwable, T]): Unit = {
      synchronized {
        value = Some(res)
        val cbs = callbacks
        callbacks = Nil
        cbs
      }.foreach(_.apply(res))
    }

    private def handler(a: Any)(cb: Callback[T]): Unit = synchronized {
      value match
        case None    => callbacks ::= cb
        case Some(v) => cb(v)
    }

    val async: Async[Any, T] = new Async(handler)
  }

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
        cb: Expr[Callback[B]]
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
