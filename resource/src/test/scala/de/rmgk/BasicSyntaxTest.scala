package de.rmgk

import de.rmgk.resource.*

class BasicSyntaxTest extends munit.FunSuite:

  class Box[T](val v: T) extends Resource { type Type = T }

  object BoxAccess extends ResourceContext[Box[?]] {
    override def accessResource(res: Box[?]): res.Type = res.v
  }

  inline def collect[Res](inline expr: Res): (List[Box[?]], BoxAccess.type => Res) = collectResources(expr)

  test("basic syntax"):
    val a = Box(2)
    val b = Box(3)

    val (boxes, fun) = collect:
      a.value + b.value

    assertEquals(boxes, List(a, b))
    assertEquals(fun(BoxAccess), 5)
