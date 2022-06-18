package de.rmgk

import de.rmgk.delay.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import concurrent.ExecutionContext.global

@main
def run() =
  inline def res2 = Async {
    val a = Future {
      println(s"running future")
      Random.nextInt()
    }.await
    def of = Future{
      println(s"in another future, a was $a")
    }
    of.await
    Sync { println("just for show") }.run
    val b = Sync { println("runs later"); math.pow(2, 10) }.run
    val g = Future{ a % b }.await
    g
  }

  println(printCode(res2))

  println("runs first")
  println(res2.run(println)(using global))
