package de.rmgk

import de.rmgk.Chain.*

import scala.collection.immutable.Iterable
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, mutable}

enum Chain[+A] extends Iterable[A], IterableOps[A, Chain, Chain[A]], IterableFactoryDefaults[A, Chain] {

  case nil                                        extends Chain[Nothing]
  case one[A](elem: A)                            extends Chain[A]
  case Wrap[A](underlying: Iterable[A])           extends Chain[A]
  case Concat[A](left: Chain[A], right: Chain[A]) extends Chain[A]

  def iterator: Iterator[A] = this match
    case `nil`               => Iterator.empty
    case one(a)              => Iterator.single(a)
    case Wrap(seq)           => seq.iterator
    case Concat(left, right) => left.iterator concat right.iterator

  override def iterableFactory: IterableFactory[Chain] = Chain

  def +:[B >: A](other: B): Chain[B]                             = Concat(one(other), this)
  def :+[B >: A](other: B): Chain[B]                             = Concat(this, one(other))
  override def concat[B >: A](suffix: IterableOnce[B]): Chain[B] = Concat(this, Chain.from(suffix))
  override def ++:[B >: A](that: IterableOnce[B]): Chain[B] = Concat(Chain.from(that), this)

}

object Chain extends IterableFactory[Chain] {
  override def from[A](source: IterableOnce[A]): Chain[A] = source match
    case c: Chain[A] => c
    case _           => Chain.Wrap(Iterable.from(source))
  override def empty[A]: Chain[A]                          = Chain.nil
  override def newBuilder[A]: mutable.Builder[A, Chain[A]] = List.newBuilder.mapResult(Chain.Wrap.apply)
}
