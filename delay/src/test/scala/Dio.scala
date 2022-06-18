package de.rmgk

import de.rmgk.delay.*

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.util.Random

import concurrent.ExecutionContext.global

@main
def run() =
  inline def res2 = Async[Unit, Int] {
    val a = Async.fromCallback[Unit, Int] { cb =>
      Future {
        println(s"running future")
        Random.nextInt()
      }(global).onComplete(t => cb(t.toEither))(global)
    }.await
    Sync { println("just for show") }.run
    val b = Sync {println("runs later"); 2 }.run
    a + b
  }

  println(printCode(res2))

  println("runs first")
  println(res2.handleInCtx(())(println))
