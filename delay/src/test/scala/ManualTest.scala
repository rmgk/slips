package de.rmgk

import de.rmgk.delay.{Async, Sync, syntax}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

@main
def run() =

  val outer: Async[Any, Unit] = Sync(())

  inline def example = {
    class ExampleResource {
      @volatile var _isOpen: Boolean = true
      def isOpen: Boolean            = synchronized(_isOpen)
      def close() = synchronized:
        _isOpen = false
    }

    val sources = List("long", "strings")

    val producer = Async[ExecutionContext] {
      val resource = Async[Any](new ExampleResource()).bind

      val jobs = AtomicInteger(sources.length)

      val products = Async.fromCallback {
        sources.foreach { item =>
          Future:
            // can only do operation (`reverse`) while resource is available
            if resource.isOpen
            then item.reverse
            else throw IllegalStateException("resource closed")
          .toAsync
        }
      }.bind

      if jobs.decrementAndGet() == 0 then resource.close()

      products

    }
  }

  println(s"CODE -------------------------------")
  println(printCode(example))
  println(s"RESULT ------------------------------")
  println(example)
  println(s"THE END -----------------------------")
