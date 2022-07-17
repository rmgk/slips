package de.rmgk

import de.rmgk.resources.*

class BasicSyntaxTest extends munit.FunSuite {

  case class Id[A](value: A) {
    def zip[B](other: Id[B]): Id[(A, B)] = Id((value, other.value))
  }

  class Box[T](val v: T) extends Resource { type Type = T }

  object BoxAccess extends ResourceContext[Box[_]] {
    override def access(res: Box[_]): res.Type = res.v
  }

  test("basic syntax") {

    val a = Box(2)
    val b = Box(3)

    val (boxes, fun) = getResources[Int, Box[_], BoxAccess.type] {
      a.value + b.value
    }

    assertEquals(boxes, List(b, a, Box(4)))
    assertEquals(fun(BoxAccess), 5)

  }

}
