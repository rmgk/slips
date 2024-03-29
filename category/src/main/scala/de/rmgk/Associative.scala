package de.rmgk

import scala.annotation.targetName
import scala.collection.IterableOps
import scala.compiletime.summonAll
import scala.deriving.Mirror

import math.Numeric.Implicits.infixNumericOps

/** A Semigroup */
trait Associative[A] {
  // the reason for the duplication is that extensions do not work with SAM
  def combine(left: A, right: A): A
  extension (left: A) @targetName("combineExt") infix def combine(right: A): A = this.combine(left, right)
}

object Associative {
  def combine[A: Associative](left: A, right: A): A = left combine right

  given optionAssoc[V: Associative]: Associative[Option[V]] =
    case (Some(l), Some(r)) => Some(l combine r)
    case (None, r)          => r
    case (l, _)             => l

  given mapAssoc[K, V: Associative]: Associative[Map[K, V]] = (left, right) =>
    right.foldLeft(left) { case (acc, (k, r)) =>
      acc.updatedWith(k) { l => l combine Some(r) }
    }

  given numericAssoc[V: Numeric]: Associative[V] = _ + _

  given iterableAssoc[A, T[U] <: IterableOps[U, T, T[U]]]: Associative[T[A]] = _ concat _

  given stringAssoc: Associative[String] = _ + _

  inline given tupleAssoc[T <: Tuple: Mirror.ProductOf]: Associative[T] = derived

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): Associative[T] =
    val instances =
      summonAll[Tuple.Map[pm.MirroredElemTypes, Associative]].toIArray.map(_.asInstanceOf[Associative[Any]])
    ProductAssociative(pm, instances)

  class ProductAssociative[T <: Product](pm: Mirror.ProductOf[T], semigroups: Seq[Associative[Any]])
      extends Associative[T] {
    override def combine(left: T, right: T): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = semigroups.length
        def productElement(i: Int): Any  = semigroups(i).combine(left.productElement(i), right.productElement(i))
      })
  }

}
