package de.rmgk

import de.rmgk.delay.{Async, Sync, syntax}

import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.util.Random

@main
def run() =

  val outer: Async[Any, Unit] = Sync(())

  inline def example = {
    Async[Any][Unit] {
      { () =>
        outer.bind
        ()
      }.apply()
    }
  }

  println(s"CODE -------------------------------")
  println(printCode(example))
  println(s"RESULT ------------------------------")
  println(example)
  println(s"THE END -----------------------------")
