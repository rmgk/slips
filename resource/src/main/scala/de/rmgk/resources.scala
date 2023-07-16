package de.rmgk

import scala.quoted.*

object resources {

  trait Resource {
    type Type
    @scala.annotation.compileTimeOnly("Resource.value may only be called within applicative macro")
    def value: Type = ???
  }

  trait ResourceContext[Res <: Resource] {
    def accessResource(res: Res): res.Type
  }

  inline def collectResources[Result, Res <: Resource, Context <: ResourceContext[Res]](inline expr: Result)
      : (List[Res], Context => Result) =
    ${ resourceMacro[Result, Res, Context]('{ expr }) }

  def resourceMacro[Res: Type, ReSource: Type, ResourceContext: Type](
      expr: Expr[Res]
  )(using q: Quotes): Expr[(List[ReSource], ResourceContext => Res)] =
    import q.reflect.*
    MacroLego[ReSource, ResourceContext].getResources[Res](expr).asInstanceOf[Expr[(
        List[ReSource],
        ResourceContext => Res
    )]]

  class MacroLego[ReSource: Type, ResourceContext: Type](using val quotes: Quotes) {

    import quotes.reflect.*

    class FindDefs extends TreeAccumulator[List[Symbol]] {
      override def foldTree(acc: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] =
        val accd = tree match {
          case d: Definition => d.symbol :: acc
          case b: Bind       => b.symbol :: acc
          case other         => acc
        }
        foldOverTree(accd, tree)(owner)
    }

    class ContainsSymbol(defs: List[quotes.reflect.Symbol]) extends TreeAccumulator[Boolean] {
      override def foldTree(x: Boolean, tree: Tree)(owner: Symbol): Boolean =
        if defs.contains(tree.symbol) then true
        else foldOverTree(x, tree)(owner)
    }

    class FindResource() extends TreeAccumulator[(List[Term], Boolean)] {
      override def foldTree(
          acc: (List[quotes.reflect.Term], Boolean),
          tree: quotes.reflect.Tree
      )(owner: quotes.reflect.Symbol): (List[quotes.reflect.Term], Boolean) = {

        def handleFind(x: Term): (List[Term], Boolean) =
          val before = acc._1
          val res    = foldTree((Nil, true), x)(owner)
          // we do not find things with nested things inside
          if (res._1.nonEmpty) then (before, false)
          else (x :: before, acc._2)

        if !tree.isExpr then foldOverTree(acc, tree)(owner)
        else
          tree.asExpr match
            case '{ (${ x }: Resource).value } => handleFind(x.asTerm)
            case _                             => foldOverTree(acc, tree)(owner)

      }
    }

    class ReplaceInterp(replacement: Map[Term, Term], ticket: Term) extends TreeMap {

      override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term = {
        def replaceAccess(xy: Term): Term = {
          replacement.get(xy) match
            case Some(replaced) => Apply(
                Select.unique(ticket, "accessResource"),
                List(replaced)
              )
            case None => report.errorAndAbort("can not access resources depending on other resources", xy.pos)
          end match
        }

        val res = if !tree.isExpr then super.transformTerm(tree)(owner)
        else
          tree.asExpr match {
            case '{ (${ xy }: Resource).value } => replaceAccess(xy.asTerm)
            case _                              => super.transformTerm(tree)(owner)
          }
        res
      }
    }

    def getResources[Res: Type](expr: Expr[Res]): Expr[Any] = {
      val fi                = FindResource().foldTree((Nil, true), expr.asTerm)(Symbol.spliceOwner)
      val foundAbstractions = fi._1
      val definitions       = FindDefs().foldTree(Nil, expr.asTerm)(Symbol.spliceOwner)

      val found = foundAbstractions.filterNot { fa =>
        val defInside      = FindDefs().foldTree(Nil, fa)(Symbol.spliceOwner)
        val containsSymbol = ContainsSymbol(definitions.diff(defInside))
        containsSymbol.foldTree(false, fa)(Symbol.spliceOwner)
      }

      val funType = MethodType.apply(List("context"))(
        (_: MethodType) => List(TypeRepr.of[ResourceContext]),
        (_: MethodType) => TypeRepr.of[Res]
      )

      val res = ValDef.let(Symbol.spliceOwner, found) { defs =>
        val replacementMap = found.zip(defs).toMap
        // val rdef = DefDef(exprSym, {params =>
        val rdef = Lambda(
          Symbol.spliceOwner,
          funType,
          { (sym, params) =>
            val ctx = params.head.asInstanceOf[Term]
            ReplaceInterp(replacementMap, ctx).transformTree(expr.asTerm)(sym)
          }
        )

        '{
          (
            List.from(${ Expr.ofList(defs.map(_.asExprOf[ReSource])) }),
            ${ rdef.asExprOf[ResourceContext => Res] }
          )
        }.asTerm
      }.asExpr

      res
    }

  }

}
