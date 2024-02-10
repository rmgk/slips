package de.rmgk

import java.util.concurrent.CompletionStage
import scala.annotation.{compileTimeOnly, unused}
import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.{Expr, Quotes, Type}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try, boundary}

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
  *   // this runs on whatever thread runs the outer async
  *   async1.runToFuture.map(x => x + 1).toAsync.bind
  *   // `toAsync` above take an execution context, and from this point onward, we are executing on one of those worker threads
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
        (cb: Callback[A]) =>
          try
            val async = syntax(expr(using ctx))
            async.run(using ctx)(cb)
          catch case e if NonFatal(e) => cb.fail(e)
      )

    /** Enables the use of [[Async.bind]] inside a sequential looking block. */
    inline def syntax[A](inline expr: A): Async[Ctx, A] =
      ${ DelayMacros.newAsyncImpl[Ctx, A]('{ expr }) }

    /** Simple form of resource handling given an `open` and `close` function for some resource.
      * Makes the resource available inside the async `body` and closes it after any single flow (value or error) was produced by `body`.
      */
    inline def resource[R, A](inline open: R, inline close: R => Unit)(inline body: Ctx ?=> R => A): Async[Ctx, A] =
      Async[Ctx]:
        val r: R = open
        Async[Ctx]:
          body(r)
        .transform: res =>
          close(r)
          Sync(res.get)
        .bind

    /** Execute a side effect when the current flow is done */
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

  /** Simple promise implementation synchronizing on the object monitor.
    * Promises guarantee that listeners receives at most one flow, even if the input has multiple flows.
    * There is no guarantee made, that all listeners receive the same flow.
    */
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

    private def handler(@unused a: Any)(cb: Callback[T]): Unit =
      synchronized:
        value match
          case None    => callbacks ::= cb; None
          case Some(v) => Some(v)
      .foreach(cb.complete)

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

  object DelayMacros {

    /** Equivalent to `sync.runInContext(ctx)` but tries to cleanup the expression assuming there are multiple inlined calls present.
      * Primarily enables beta reduction in case sync is just `new Sync(..)` (which happens a lot due to inlining)
      */
    def applyInBlock[Ctx: Type, B: Type](
        sync: Expr[Sync[Ctx, B]],
        ctx: Expr[Ctx]
    )(using quotes: Quotes): Expr[B] = {
      import quotes.reflect.*
      transformLast(sync.asTerm) { term =>
        Expr.betaReduce:
          term.asExpr match
            case '{ new Sync[Ctx, B]($scxfun) } =>
              '{ $scxfun.apply($ctx) }
            case '{ $other: Sync[Ctx, B] } =>
              '{ $other.runInContext($ctx) }
        .asTerm
      }.asExprOf[B]
    }

    /** Similar to `applyInBlock` but for `async.handleInCtx(ctx)(cb)`.
      * However, it does not have much special handling to clean up sequential callbacks.
      * In particular, a pattern like this is quite common:
      * ```
      *   def anonfunction(res: Try[_]) = try ... catch ... // (transform handler)
      *   val callback: Callback = anonfunction
      *   callback.complete(Success(...))
      * ```
      *
      * To improve legibility of the generated code, it would be desirable to simplify and inline further, including removing the try catch in cases where there is an outer handler.
      * I don’t think there would be much of a performance difference (because of anything that uses async being way less efficient anyway).
      */
    def handleInBlock[Ctx: Type, B: Type](
        async: Expr[Async[Ctx, B]],
        ctx: Expr[Ctx],
        cb: Expr[Callback[B]]
    )(using quotes: Quotes): Expr[Unit] = {
      import quotes.reflect.*
      transformLast(async.asTerm) {
        term =>
          Expr.betaReduce:
            term.asExpr match
              case '{ new Async[Ctx, B]($scxfun) } =>
                '{ $scxfun.apply($ctx).apply($cb) }
              case '{ new Sync[Ctx, B]($scxfun) } =>
                '{ $cb.succeed($scxfun($ctx)) }
              case '{ $other: Async[Ctx, B] } =>
                '{ $other.handleInCtx($ctx)($cb) }
          .asTerm
      }.asExprOf[Unit]
    }

    def transformLast[A](using
        quotes: Quotes
    )(expr: quotes.reflect.Statement)(transform: quotes.reflect.Statement => quotes.reflect.Term)
        : quotes.reflect.Term = {
      import quotes.reflect.*
      def rec(expr: Statement): Term =
        expr match
          case Inlined(a, b, t)                                 => Inlined(a, b, rec(t))
          case Match(_, List(CaseDef(Wildcard(), None, inner))) => rec(inner)
          case Block(statements, expr) =>
            Block(statements, rec(expr))
          case NamedArg(a, expr) => NamedArg(a, rec(expr))
          // throw away type annotations as we transform the terms to a different type so they become invalid
          case Typed(expr, tt) => rec(expr)
          case other           => transform(other)
      rec(expr)
    }

    def transformLastOption[A](using quotes: Quotes)(
        expr: quotes.reflect.Statement,
        default: quotes.reflect.Term
    )(
        transform: PartialFunction[quotes.reflect.Statement, quotes.reflect.Term]
    ): quotes.reflect.Term = {
      boundary[quotes.reflect.Term]:
        transformLast(expr) { arg =>
          transform.lift(arg) match
            case None    => boundary.break(default)
            case Some(v) => v
        }

    }

    def newAsyncImpl[Ctx: Type, T0: Type](expr: Expr[T0])(using quotes: Quotes): Expr[Async[Ctx, T0]] = {
      import quotes.reflect.*

      case class RecRes[T](term: Expr[Async[Ctx, T]], binds: Boolean)

      def handleSubBlockOfType[T: Type](expr: Expr[T]): RecRes[T] = {

        /** Applies `withBound` to the result of `async` (once that produces values)
          * In other words, this essentially just does `async.flatMap(withBound)`,
          * but works with the pieces directly to hopefully be a bit more reliable in this AST manipulation context
          */
        def doBind[R: Type](async: Term)(withBound: Term => Expr[Async[Ctx, R]]): Expr[Async[_ <: Ctx, R]] =
          async.tpe.asType match {
            case '[Async[γ, α]] =>
              if !(TypeRepr.of[Ctx] <:< TypeRepr.of[γ])
              then
                report.errorAndAbort(
                  s"Can only bind matching context, but »${Type.show[Ctx]}« is not a subtype of »${Type.show[γ]}« ",
                  async.asExpr
                )
              else
                '{
                  ${ async.asExprOf[Async[γ, α]] }.flatMap {
                    (v: α) => ${ withBound('{ v }.asTerm) }
                  }
                }
            case '[other] =>
              report.errorAndAbort(
                s"Expected Async[${Type.show[Ctx]}, ?], but got ${Type.show[other]}«",
                async.asExpr
              )
          }

        case class StateRes(binds: Boolean, statement: Statement)
        def handleStatement(statement: Statement, continuation: Option[Expr[Async[Ctx, T]]]): StateRes = {
          report.info(s"------------ handling statement\n${statement.show}\n")
          statement match
            // handle val def that end with a bind, like:
            // val x = {
            //   println(s"test")
            //   async.bind
            // }
            // Note that CleanBlock treats expressions as blocks with an empty list of statements
            case vd @ ValDef(name, typeTree, Some(inner)) =>
              println(s"VAL DEF ${vd.show}")
              inner.asExpr match
                case '{ ($async: Async[Ctx, α]).bind } =>
                  StateRes(
                    true, {
                      doBind(async.asTerm) { v =>
                        Block(
                          List(ValDef.copy(vd)(name, typeTree, Some(v))),
                          continuation.get.asTerm
                        ).asExprOf[Async[Ctx, T]]
                      }.asTerm
                    }
                  )
                case other =>
                  typeTree.tpe.asType match
                    case '[τ] =>
                      val res = handleSubBlockOfType[τ](inner.asExprOf[τ])
                      println(s"WSA BAD BUT BETTER: ${res.binds}\n(${inner.show})\n${res.term.show}\n-----------tt")
                      if res.binds
                      then
                        val updated =
                          ValDef.copy(vd)(name, typeTree, Some('{ ${ res.term.asExprOf[Async[Ctx, _]] }.bind }.asTerm))
                        handleStatement(updated, continuation)
                      else StateRes(false, statement)

            case block @ Block(_, _) =>
              val res = handleSubBlockOfType(block.asExpr)
              if !res.binds then StateRes(false, block)
              else handleStatement('{ ${ res.term.asExprOf[Async[Ctx, _]] }.bind }.asTerm, continuation)
            case other: Term =>
              other.asExpr match
                case '{ ($other: Async[Ctx, α]).bind } =>
                  StateRes(
                    true,
                    doBind(other.asTerm) { v =>
                      continuation match
                        case Some(value) => value
                        case None        => '{ Sync[Ctx](${ v.asExprOf[T] }) }
                    }.asTerm
                  )
                case otherExpr =>
                  report.info(s"not an async: ${other.show}")
                  StateRes(false, other)

            case somethingElseEntirely =>
              report.errorAndAbort(
                s"cannot handle as async\n(${somethingElseEntirely.asInstanceOf[Term].tpe.show}) \n${somethingElseEntirely.show}"
              )

        }

        def handleTerminal(expr: Term): RecRes[T] = {
          val handled: StateRes = handleStatement(expr, None)
          val packedResult =
            if handled.binds
            then handled.statement.asExprOf[Async[Ctx, T]]
            else
              println(s"DOES NOT BIND\n${expr.show}")
              report.info(s"fixing\n${expr.show}")
              '{ Sync[Ctx](${ expr.asExprOf[T] }) }
          RecRes(packedResult, handled.binds)
        }

        def rec(term: Term): RecRes[T] =
          println(s"+++++++++++++++++b doing recursion\n${term.show}\n")
          term match
            case Inlined(_, _, t) => rec(t)
            case Block(statements, expr) =>
              println(s"BLOCK ${term.show}")
              val terminalRes = handleTerminal(expr)
              terminalRes.term match
                case '{ $transformed: Async[Ctx, T] } =>
                  report.info(
                    s"--------------------term:\n${term.show}\n-------------------------before:\n${expr.show}\n-----------------------------after:\n${transformed.show}\n--------------",
                    term.asExpr
                  )

                  // we take the sequence of statements in the async block,
                  // and rewrite it into a nested list of handlers
                  statements.foldRight[RecRes[T]](RecRes[T](transformed, false)):
                    case (s, RecRes(acc, binds)) =>
                      val res = handleStatement(s, Some(acc))
                      if res.binds
                      then RecRes(res.statement.asExprOf[Async[Ctx, T]], true)
                      else RecRes(Block(List(s), acc.asTerm).asExprOf[Async[Ctx, T]], binds)
                case other =>
                  report.errorAndAbort(s"cannot handle\n${other.asTerm.tpe.show}\n${other.show}")

            case other: Term =>
              println(s"TERMINAL ${other.show}")
              handleTerminal(other)

        rec(expr.asTerm)

      }

      handleSubBlockOfType[T0](expr).term
    }

  }
}
