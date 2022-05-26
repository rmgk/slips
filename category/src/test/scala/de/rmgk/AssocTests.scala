package de.rmgk

import de.rmgk.Associative
import org.scalacheck.*
import org.scalacheck.Prop.*

class AssocTests extends munit.FunSuite {

  val text  = """Eins zwei zwei drei drei drei"""
  val words = text.split("\\W+").map(_.toLowerCase)

  case class Sum(value: Int)

  test("nested wordcount") {
    given Associative[Sum] = Associative.derived
    val res = words.map(w => Option(Map(w -> Sum(1)))).reduceOption(Associative.combine).flatten.getOrElse(Map.empty)
    assertEquals(res, Map("eins" -> Sum(1), "zwei" -> Sum(2), "drei" -> Sum(3)))
  }

  test("iterables") {
    val l              = List(1, 2, 3)
    val r              = List(4, 5)
    val c              = List(1, 2, 3, 4, 5)
    val res: List[Int] = Associative.combine(l, r)
    assertEquals(res, c)
    assertEquals(Associative.combine(l.toSet, r.toSet), c.toSet)
    assertEquals(Associative.combine(l.toSet, r.toSet), c.toSet)
  }
}

class ListAssoc extends LatticeMergeTest[List[Int]]
class SetAssoc extends LatticeMergeTest[Set[Long]]
class MapAssoc extends LatticeMergeTest[Map[Int, String]]
class NestedAssoc extends LatticeMergeTest[List[Set[Int]]]

abstract class LatticeMergeTest[A: Arbitrary: Associative] extends munit.ScalaCheckSuite {

  property("associative") {
    forAll { (a: A, b: A, c: A) =>
      val ab   = Associative.combine(a, b)
      val bc   = Associative.combine(b, c)
      val abc  = Associative.combine(ab, c)
      val abc2 = Associative.combine(a, bc)
      assertEquals(abc, abc2)
    }
  }
}
