package de.rmgk

import de.rmgk.delay.*

import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.util.Random

@main
def run() =
  inline def res2 = Async {
    val a = Future {
      println(s"running future")
      Random.nextInt()
    }.await
    def of = Future { println(s"in another future, a was $a") }
    Sync { println("just for show") }.run
    of.await
    val b = Sync { println("runs later"); math.pow(2, 10) }.run
    Future { a % b }.await
  }

  println("runs first")
  res2.run(println)(using global)

  println(printCode {
    Async {
      Async { 123 }.await
      4
    }
  })
