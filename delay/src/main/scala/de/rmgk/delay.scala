package de.rmgk

import java.util.concurrent.CompletionStage
import scala.annotation.{compileTimeOnly, unused}
import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.{Expr, Quotes, Type}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Work with descriptions of computations that you want to execute later.
  * Delay offers two abstractions [[Sync]] and [[Async]].
  *
  * [[Sync]] which behaves like a function `() => A` its only purpose is to enable much more efficient composition by using macros to transform any [[delay.extensions.map]] and [[delay.extensions.flatMap]] calls into just sequential code if everything is inlined, while falling back to passing around function pointers if you pass the [[Sync]] as parameters.
  * [[Sync]] is particularly useful for composition of code with side effects.
  *
  * [[Async]] similarly allows composition of asynchronous computations.
  * It behaves similarly to a `(A => Unit) => Unit` (i.e., a function that takes a callback).
  * The purpose of [[Async]] is not only efficiency, but also to provide an alternative syntax to for expressions.
  * Specifically, async blocks allow sequential looking composition of async code:
  * ```
  * Async {
  *   val x = async1.bind
  *   async2.bind
  *   println("output")
  *   val y = async3.bind
  *   x + y
  * }
  * ```
  * Which is roughly equivalent to:
  * ```
  * for {
  *   x <- async1
  *   _ <- async2
  *   y <- {
  *     println("output")
  *     async3
  *   }
  * } yield { x + y }
  * ```
  * Thus just a bit more regular than for expressions. Note that you may not use bind in any nested expressions, only as the outer method in a single statement, or as the right hand side of a val binding.
  *
  * Note that [[Sync]] and in particular [[Async]] are just abstractions, there is no additional runtime or internal state beyond composition of functions. In particular, [[Async]] has no concept of parallel execution or threads or anything like that. Using [[delay.extensions.run]] (and thus at some point [[Async.bind]]) simply executes whatever  code you have written in the async handlers and likely the initial [[AsyncCompanion.fromCallback]].
  *
  * If you need to execute anything on a different thread you can easily do so by using whatever library for threading you fancy. We provide convenience functions to convert to and from [[scala.concurrent.Future]]:
  *
  * ```
  * Async {
  *   async1.runToFuture.map(x => x + 1).toAsync.bind
  * }
  * ```
  */
object delay {

  /** Description of a computation that returns an `A` immediately without blocking.
    * This could be seen as a `() => A` but with an additional `Ctx` to store context information.
    */
  class Sync[-Ctx, +A](val runInContext: Ctx => A) extends Async[Ctx, A](ctx => cb => cb.succeed(runInContext(ctx)))

  /** Companion object of [[Sync]], but with a type parameter to allow fine grained type inference.
    * This could potentially be simplified if we ever get named type parameters.
    */
  class SyncCompanion[Ctx]:
    inline def apply[A](inline run: Ctx ?=> A): Sync[Ctx, A] = new Sync(x => run(using x))
  inline def Sync[Ctx]: SyncCompanion[Ctx] = new SyncCompanion[Ctx] {}

  /** A callback that also handles failure. */
  @FunctionalInterface
  trait Callback[-A] {
    inline def succeed(value: A): Unit   = complete(Success(value))
    inline def fail(ex: Throwable): Unit = complete(Failure(ex))
    def complete(tr: Try[A]): Unit
    inline def complete(res: Either[Throwable, A]): Unit = complete(res.toTry)
  }

  /** A description of a computation that returns an `A` at some later point in time.
    * This could be seen as a `(A => Unit) => Unit` but with error handling and an additional context to store some information threaded through the asynchronous execution.
    * You probably do not want to execute this directly
    */
  class Async[-Ctx, +A](val handleInCtx: Ctx => Callback[A] => Unit) {

    /** Access the value inside an Async block. */
    @compileTimeOnly("bind must be used inside Async and may not be nested inside of expressions")
    def bind: A = ???
  }

  /** Companion object of [[Async]], but with a type parameter to allow fine grained type inference.
    * This could potentially be simplified if we ever get named type parameters.
    */
  trait AsyncCompanion[Ctx]:

    /** Create a new [[Async]] from a callback.
      * `f` is a block that may pass [[handler]] to a callback-based API.
      * @example ```
      * Async.fromCallback {
      *   fut.onComplete(Async.handler.complete(_))
      * }
      * ```
      */
    inline def fromCallback[A](inline f: Ctx ?=> Callback[A] ?=> Unit): Async[Ctx, A] =
      new Async(ctx => cb => f(using ctx)(using cb))

    /** Use inside a [[fromCallback]] */
    inline def handler[A](using cb: Callback[A]): Callback[A] = cb

    /** Main async syntax. The body `expr` is not executed until the returned [[Async]] is started with [[delay.extensions.run]] or one of the variants. Any exceptions raised in `expr` are forwarded to the handler of the async run. */
    inline def apply[A](inline expr: Ctx ?=> A): Async[Ctx, A] =
      new Async[Ctx, A](ctx =>
        cb =>
          try syntax(expr(using ctx)).run(using ctx)(cb)
          catch case e if NonFatal(e) => cb.fail(e)
      )

    /** Enables the use of [[Async.bind]] inside a sequential looking block. */
    inline def syntax[A](inline expr: A): Async[Ctx, A] =
      ${ DelayMacros.asyncImpl[Ctx, A]('{ expr }) }

    /** Simple form of resource handling given an `open` and `close` function for some resource.
      * Makes the resource available inside the async `body` and ensures it is closed as soon as `body` is done (exceptionally or normally).
      */
    inline def resource[R, A](inline open: R, inline close: R => Unit)(inline body: Ctx ?=> R => A): Async[Ctx, A] =
      Async[Ctx]:
        val r = open
        Async[Ctx]:
          body(r)
        .transform: res =>
          close(r)
          Sync(res.get)
        .bind

    inline def defer[R, A](inline close: Unit): Async[Ctx, Unit] =
      Async.fromCallback:
        try Async.handler.succeed(())
        finally close

    /** `.provide`, but you can put the context first. */
    inline def provided[Ctx, A](ctx: Ctx)(inline expr: Ctx ?=> A): Async[Any, A] =
      Async { expr }.provide(ctx)

    /** While we don’t do bind inside of nested expressions, if the expression returns an async we can bind it.
      * Sometimes, having this as a block is syntactically more convenient, i.e:
      * ```
      * Async:
      *   bind:
      *     if random > 4
      *     then Async(...)
      *     else Async(...)
      * ```
      */
    inline def bind[Ctx, A](inline a: Async[Ctx, A]): A = a.bind
  end AsyncCompanion

  /** Syntactic convenience to enable type inference of `Ctx`. */
  inline def Async[Ctx]: AsyncCompanion[Ctx] = new AsyncCompanion[Ctx] {}

  /** Simple promise implementation synchronizing on the object monitor. */
  class Promise[T] extends Callback[T]:
    @volatile private var value: Option[Try[T]]        = None
    @volatile private var callbacks: List[Callback[T]] = Nil

    override def complete(res: Try[T]): Unit =
      synchronized:
        value = Some(res)
        val cbs = callbacks
        callbacks = Nil
        cbs
      .foreach(_.complete(res))

    private def handler(@unused a: Any)(cb: Callback[T]): Unit = synchronized:
      value match
        case None    => callbacks ::= cb
        case Some(v) => cb.complete(v)

    val async: Async[Any, T] = new Async(handler)
  end Promise

  implicit object syntax:

    extension [Ctx, A](inline sync: Sync[Ctx, A]) {

      /** Executes the [[Sync]] given a `Ctx`. */
      inline def run(using inline ctx: Ctx): A          = ${ DelayMacros.applyInBlock[Ctx, A]('{ sync }, '{ ctx }) }
      inline def map[B](inline f: A => B): Sync[Ctx, B] = Sync { f(sync.run) }
      inline def flatMap[B](inline f: A => Sync[Ctx, B]): Sync[Ctx, B] = Sync { f(sync.run).run }
    }

    extension [Ctx, A](inline async: Async[Ctx, A])

      /** Start the underlying computation and pass the result to `cb`. */
      inline def run(using inline ctx: Ctx)(inline cb: Callback[A]): Unit =
        ${ DelayMacros.handleInBlock[Ctx, A]('{ async }, '{ ctx }, '{ cb }) }

      /** Provides a new context to the async. */
      inline def provide[Outer](inline part: Outer ?=> Ctx): Async[Outer, A] =
        Async[Outer].fromCallback[A] { (outer: Outer) ?=> (cb: Callback[A]) ?=>
          async.run(using part(using outer))(cb)
        }

      /** Transform successes. Propagate failures. */
      inline def map[B](inline f: Ctx ?=> A => B): Async[Ctx, B] =
        async.flatMap { a => Sync { f(a) } }

      /** Transform successes. Propagate failures. */
      inline def flatMap[B](inline f: Ctx ?=> A => Async[Ctx, B]): Async[Ctx, B] =
        transform { a => f(a.get) }

      /** replaces the async execution with some other async execution in case fo an exception */
      inline def recover(inline f: Ctx ?=> Throwable => Async[Ctx, A]): Async[Ctx, A] =
        transform:
          case Success(a)   => Sync(a)
          case Failure(err) => f(err)

      /** Allows to handle both success and failure cases.
        * More or less a generalized version of `flatMap`
        */
      inline def transform[B](inline f: Ctx ?=> Try[A] => Async[Ctx, B]): Async[Ctx, B] = Async.fromCallback:
        async.run: res =>
          try f(res).run(Async.handler)
          catch
            case e if NonFatal(e) =>
              res.recover(e.addSuppressed(_))
              Async.handler.fail(e)

      /** Start the underlying computation immediately.
        * Return a Future of the result.
        */
      inline def runToFuture(using inline ctx: Ctx): Future[A] =
        val p = scala.concurrent.Promise[A]()
        async.run(p.complete)
        p.future

      /** Start the underlying computation immediately.
        * The result is cached and can be accessed as Async
        */
      inline def runToAsync(using inline ctx: Ctx): Async[Ctx, A] =
        val p = new Promise[A]
        async.run(p)
        p.async
    end extension

    extension [A](inline fut: Future[A])
      inline def toAsync(using inline ec: ExecutionContext): Async[Any, A] =
        Async.fromCallback:
          fut.onComplete(Async.handler.complete)

    extension [T](inline cs: CompletionStage[T])
      inline def toAsync: Async[Any, T] = Async.fromCallback:
        cs.whenComplete: (res, ex) =>
          if null != ex then Async.handler.fail(ex)
          else if null != res then Async.handler.succeed(res)
          else Async.handler.fail(IllegalStateException("completion stage returned nothing without failure"))
        () // cs.whenComplete returns kinda itself … it’s a bit weird, but I think it is safe to ignore
  end syntax

  object DelayMacros:
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
                  Expr.betaReduce('{ $cb.succeed($scxfun($ctx)) }).asTerm
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

      def blockedIO(stmts: List[Statement], io: Term)(withBound: Term => Expr[Async[Ctx, T]]) =
        io.tpe.asType match {
          case '[Async[Ctx, α]] =>
            '{
              ${ (if stmts.isEmpty then io else Block(stmts, io)).asExprOf[Async[Ctx, α]] }.flatMap {
                (v: α) => ${ withBound('v.asTerm) }
              }
            }
          case '[Async[γ, α]] => report.errorAndAbort(
              s"Can only bind matching context, but »${Type.show[γ]}« is not »${Type.show[Ctx]}«",
              io.asExpr
            )
        }

      cleanBlock(expr.asTerm) match {
        case block @ Block(statements: List[Statement], expr) =>
          val Block(List(stmt), init) = ValDef.let(Symbol.spliceOwner, "async$macro$result", expr) { ref =>
            '{ Sync[Ctx][T] { ${ ref.asExprOf[T] } } }.asTerm
          }: @unchecked
          (statements :+ stmt).foldRight[Term](init) { (s, acc) =>
            s match {
              case vd @ ValDef(name, typeTree, Some(CleanBlock(Block(stmts, Select(io, "bind"))))) =>
                blockedIO(stmts, io) { v =>
                  Block(
                    List(ValDef.copy(vd)(name, typeTree, Some(v))),
                    acc
                  ).asExprOf[Async[Ctx, T]]
                }.asTerm
              case CleanBlock(Block(stmts, Select(io, "bind"))) =>
                blockedIO(stmts, io) { _ => acc.asExprOf[Async[Ctx, T]] }.asTerm
              case _ =>
                Block(List(s), acc)
            }
          }.asExprOf[Async[Ctx, T]]
        case other =>
          '{ Sync[Ctx][T]($expr) }
      }
    }
  end DelayMacros

}
