package de.rmgk

import scala.quoted.*
import scala.Tuple
import scala.Function.chain

object resources {

  trait Resource {
    type Type
    @scala.annotation.compileTimeOnly("Resource.value may only be called within applicative macro")
    def value: Type = ???
  }

  trait ResourceContext[Res <: Resource] {
    def access(res: Res): res.Type
  }

  inline def getResources[Result, ReSource, ResourceContext](inline expr: Result)
      : (List[ReSource], ResourceContext => Result) =
    ${ resourceMacro[Result, ReSource, ResourceContext]('expr) }

  def resourceMacro[Res: Type, ReSource: Type, ResourceContext: Type](
      expr: Expr[Res]
  )(using q: Quotes): Expr[(List[ReSource], ResourceContext => Res)] =
    import q.reflect.*
    MacroLego[ReSource, ResourceContext].makeReactive[Res](expr).asInstanceOf[Expr[(
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
      import quotes.reflect.*

      override def foldTree(x: Boolean, tree: Tree)(owner: Symbol): Boolean =
        if defs.contains(tree.symbol) then true
        else foldOverTree(x, tree)(owner)
    }

    class FindResource() extends TreeAccumulator[(List[Term], Boolean)] {

      override def foldTree(
          acc: (List[quotes.reflect.Term], Boolean),
          tree: quotes.reflect.Tree
      )(owner: quotes.reflect.Symbol): (List[quotes.reflect.Term], Boolean) = {

        if !tree.isExpr then foldOverTree(acc, tree)(owner)
        else
          tree.asExpr match
            case '{ (${ x }: Resource).value } =>
              val before = acc._1
              val res    = foldTree((Nil, true), x.asTerm)(owner)
              // we do not find things with nested things inside
              if (res._1.nonEmpty) then (acc._1, false)
              else (x.asTerm :: acc._1, acc._2)
            case _ => foldOverTree(acc, tree)(owner)
      }
    }

    class ReplaceInterp(replacement: Map[Term, Term], ticket: Term) extends TreeMap {

      override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term = {
        def accessTree(accessed: Term): Term = Apply(
          Select.unique(ticket, "access"),
          List(accessed)
        )

        def replaceAccess(xy: Term): Term = {
          replacement.get(xy) match
            case Some(replaced) => accessTree(replaced)
            case None           => report.errorAndAbort("can not access resources depending on other resources", xy.pos)
          end match
        }

        val res = if (!tree.isExpr) then super.transformTerm(tree)(owner)
        else
          tree.asExpr match {
            case '{ (${ xy }: Resource).value } => replaceAccess(xy.asTerm)
            case _                              => super.transformTerm(tree)(owner)
          }
        res
      }
    }

    def makeReactive[Res: Type](expr: Expr[Res]): Expr[Any] = {
      val fi                = FindResource().foldTree((Nil, true), expr.asTerm)(Symbol.spliceOwner)
      val foundAbstractions = fi._1
      val foundStatic       = fi._2
      val definitions       = FindDefs().foldTree(Nil, expr.asTerm)(Symbol.spliceOwner)

      val found = foundAbstractions.filterNot { fa =>
        val defInside      = FindDefs().foldTree(Nil, fa)(Symbol.spliceOwner)
        val containsSymbol = ContainsSymbol(definitions.diff(defInside))
        containsSymbol.foldTree(false, fa)(Symbol.spliceOwner)
      }

      val funType = MethodType.apply(List("ticket"))(
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
