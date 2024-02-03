package de.rmgk

import de.rmgk.delay.{Async, Sync, syntax}

import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.util.Random

@main
def run() =
  inline def res2 = Async {
    val a = Future {
      println(s"running future")
      Random.nextInt()
    }.toAsync.bind
    def of = Future { println(s"in another future, a was $a") }
    Sync { println("just for show") }.run
    of.toAsync.bind
    val b = Sync { println("runs later"); math.pow(2, 10) }.run
    Future { a % b }.toAsync.bind
  }

  println("runs first")
  res2.run(using global)(println)


  println(s"-----------------------------")

  println(printCode:
    Async:
      val x = Sync(4).run
      val y = Sync(5).bind
      x + y
  )
