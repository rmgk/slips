import de.rmgk.delay.*

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Random, Success}
import scala.util.control.ControlThrowable

class DelayTests extends munit.FunSuite {
  test("exception in async") {
    val as = Async { throw new IllegalStateException("test") }
    as.run {
      case Failure(e) => assert(e.getMessage == "test")
      case Success(_) => assert(false)
    }
  }

  test("exception nested") {
    val failed = Async { throw new IllegalStateException("test") }

    var count = 0

    val counting = Async {
      count += 1
      val success = Async { 100 }.bind
      failed.bind
      count += 1
    }

    assert(count == 0)

    counting.run {
      case Failure(e) => assert(e.getMessage == "test")
      case Success(_) => assert(false)
    }

    assert(count == 1)
  }

  test("exception nested2") {
    val failed = Async { throw new IllegalStateException("test") }

    var count = 0

    val counting = Async {
      count += 1
      val success = Async { 100 }.bind
      count += 1
      val failure = {
        throw IllegalStateException("test2")
        Async(30)
      }.bind
      count += 1
      failed.bind
      count += 1
    }

    assert(count == 0)

    counting.run {
      case Failure(e) => assert(e.getMessage == "test2")
      case Success(_) => assert(false)
    }

    assert(count == 2)
  }

  test("close") {
    var messages: List[String] = Nil
    val m1                     = s"starting something"
    val m2                     = s"does not happen anymore"
    val m3                     = "resource"
    val m4                     = "afterwards"
    val me                     = "oh noes!"

    Async[Unit] {
      Async.resource(m3, me => messages ::= me) { msg =>
        messages ::= m1
        throw IllegalStateException(me)
        messages ::= m2
      }.bind
    }
      .run(using ()) {
        case Failure(e) =>
          assert(e.getMessage == me)
          messages ::= m4
        case _ => assert(false)
      }

    assertEquals(messages, List(m4, m3, m1))
  }

  test("close in scope") {
    var messages: List[String] = Nil
    val m1                     = s"starting something"
    val m3                     = "resource closed"
    val m4                     = "afterwards"
    val m5                     = "finally"

    Async[Unit] {
      val irrelevant = Async("at least I hope so").bind
      Async.resource(m3, me => messages ::= me) { msg =>
        messages ::= m1
      }.bind

      val alsoIrrelevant = Async("at least I hope so 2").bind

      messages ::= m5
    }.run(using ()) {
      case Failure(e) =>
        assert(false, s"got exception, but was unexpected $e")
        messages ::= m4
      case _ => assert(true)
    }

    assertEquals(messages, List(m5, m3, m1))
  }

  test("defer in scope") {
    var messages: List[String] = Nil
    val m1                     = s"starting something"
    val m3                     = "resource closed"
    val m4                     = "afterwards"
    val m5                     = "finally"

    Async[Unit] {
      Async(messages ::= m1).bind

      Async.defer(messages ::= m3).bind

      val irrelevant = Async("at least I hope so 2").bind
      Async(messages ::= m4).bind

      messages ::= m5
    }.run(using ()) { _ => }

    assertEquals(messages, List(m3, m5, m4, m1))
  }

  test("defer with generator?") {
    var messages: List[String] = Nil
    val m1                     = s"starting something"
    val m3                     = "resource closed"
    val m4                     = "afterwards"
    val m5                     = "finally"

    Async[Unit] {
      Async.fromCallback:
        Async.handler.succeed(1)
        Async.handler.succeed(2)
      .bind

      messages ::= m1

      Async.defer(messages ::= m3).bind

      val irrelevant = Async("at least I hope so 2").bind
      Async(messages ::= m4).bind

      messages ::= m5
    }.run(using ()) { _ => }

    assertEquals(messages, List(m3, m5, m4, m1, m3, m5, m4, m1))
  }

  def makeFailing =
    var needed = 3
    val failsALot = Async[Any] {
      needed -= 1
      if needed >= 0
      then throw new Exception(s"not yet $needed")
      else "OK"
    }

    // this is an interesting trick, but will stackoverflow at some point
    lazy val recovering: Async[Int, String] = failsALot.recover { retries ?=> e =>
      if retries > 0
      then recovering.provide(retries - 1)
      else throw e
    }
    recovering
  end makeFailing

  test("retry counter success") {
    import concurrent.ExecutionContext.Implicits.global
    makeFailing.runToFuture(using 4).map { res =>
      assertEquals(res, "OK")
    }
  }

  test("retry counter failure") {
    import concurrent.ExecutionContext.Implicits.global
    makeFailing.runToFuture(using 2).failed
  }

  test("provide flow") {
    val res: Async[Any, String] = Async[Any] {
      Async(4).bind
      Async.provided("provided") {
        summon[String]
      }.bind
    }

    assertEquals(res.runToFuture.value, Some(Success("provided")))
  }

  test("not cached"):
    var counter = 0
    val counting = Async {
      counter = counter + 1
      counter
    }

    val indirect = Async:
      val c = counting.bind
      c * 2

    assertEquals(counter, 0, "should not execute")

    assertEquals(indirect.runToFuture.value, Some(Success(2)))
    assertEquals(indirect.runToFuture.value, Some(Success(4)))
    assertNotEquals(indirect.runToFuture.value, Some(Success(4)))

  test("cached"):
    var counter = 0
    val counting = Async {
      counter = counter + 1
      counter
    }.runToAsync

    // did immediately execute
    assertEquals(counter, 1, "should immediately execute")

    val indirect = Async:
      val c = counting.bind
      c * 2

    assertEquals(indirect.runToFuture.value, Some(Success(2)))
    assertEquals(indirect.runToFuture.value, Some(Success(2)))
    assertNotEquals(indirect.runToFuture.value, Some(Success(4)))

  object CancelControl extends ControlThrowable

  /* There is some potential here to expand this into something usable, but it would be good if the CancelControl exception could somehow be handled like boundary/break where the exception has an explicit label. I currently speculate that would require a new subtype of Async, that has this exception label stored somewhere. Actually, we could pass it on evrey invocation of .succeed below. */
  test("generate … ?"):

    val onetwo = Async.fromCallback:
      try
        var x = 0
        while
          x += 1
          x < 10
        do
          Async.handler.succeed(x)
        Async.handler.succeed(14)
      // This “works” because CancelControl is a ControlThrowable, which is not handled as normal by the error forwarding mechanism
      // It seems like a terrible idea though.
      catch case CancelControl => ()

    var seen = List.empty[Int]

    val res = Async:
      val x = onetwo.bind
      seen ::= x
      if x == 7 then throw CancelControl
    res.runToAsync
    assertEquals(seen, List(7, 6, 5, 4, 3, 2, 1))

  /* I mean, this clearly would not work with async things on other threads, because we cannot abort those with exceptions. No, wait, actually we can, because the callback runs on that thread. The only thing we then cannot do is actually catch the exception with the boundary. So we would need support from … what? Hmhm. */
  test("generate with better aborts?"):
    val onetwo = Async.fromCallback:
      try
        var x = 0
        while
          x += 1
          x < 10
        do
          Async.handler.succeed(x)
        Async.handler.succeed(14)
      catch case CancelControl => ()

    var seen = List.empty[Int]

    import scala.util.boundary
    boundary:
      val res = Async:
        val x = onetwo.bind
        seen ::= x
        if x == 7 then throw boundary.break(x)
      res.runToAsync

    // this does NOT break, because the break exception is caught and propagated
    assertEquals(seen, List(14, 9, 8, 7, 6, 5, 4, 3, 2, 1))

  test("resource and stream"):

    val many = Async.fromCallback:
      var x = 0
      while
        x += 1
        x < 4
      do
        Async.handler.succeed(x)

    var seen = List.empty[(Int, String)]

    val res = Async:
      var y = ""
      val x = many.bind
      Async.bind:
        Async.resource(y, m => { y = y + "x" }): r =>
          seen = ((x, r)) :: seen
    res.runToAsync
    assertEquals(seen, List((3, "xx"), (2, "x"), (1, "")))

  /** It’s all about co-op-per-ray-tion */
  test("generate with context cancel") {

    class Ctrl(var cancel: Boolean)

    val onetwo = Async.fromCallback:
      var x = 0
      while
        x += 1
        x < 10 && !summon[Ctrl].cancel
      do
        Async.handler.succeed(x)
      Async.handler.succeed(14)

    var seen = List.empty[Int]

    val res = Async:
      val x = onetwo.bind
      seen ::= x
      if x == 7 then summon[Ctrl].cancel = true
    val ctrl = Ctrl(false)
    res.runToAsync(using ctrl)
    assertEquals(seen, List(14, 7, 6, 5, 4, 3, 2, 1))
  }

  /** Interrupts? This works on JS, wat? */
  test("generate with thread cancel") {

    val onetwo = Async.fromCallback:
      var x = 0
      while
        x += 1
        x < 10 && !Thread.interrupted()
      do
        Async.handler.succeed(x)
      Async.handler.succeed(14)

    var seen = List.empty[Int]

    val res = Async:
      val x = onetwo.bind
      seen ::= x
      if x == 7 then Thread.currentThread().nn.interrupt()
    res.runToAsync
    assertEquals(seen, List(14, 7, 6, 5, 4, 3, 2, 1))
  }

  test("context contravariance") {

    class Fruit
    case class Apple() extends Fruit

    var seen: List[Any] = Nil

    val a = Async[Apple]:
      seen ::= summon[Apple]
    val b = Async[Fruit]:
      seen ::= summon[Fruit]
    val c = Async[Any]: ctx ?=>
      seen ::= ctx

    Async[Apple]:
      a.bind
      b.bind
      c.bind
    .run(using Apple())(_ => ())

    assertEquals(seen, List(Apple(), Apple(), Apple()))
  }

  test("bind inside subblocks") {
    val outer = Async[Any] { 100 }

    def example =
      Async {
        val a = outer.bind
        val x = 6
        val z = {
          // comment
          val r = x + 1
          val a = outer.bind
          a + x + r
        }
        val b = outer.bind
        a + b + x + z
      }
    var res = 0
    example.run:
      case Success(v) => res = v
      case other      => assert(false, "no exceptions!")

    assertEquals(res, 319)
  }

  test("extraced future example") {

    var messages: List[String] = Nil

    def res2: Async[ExecutionContext, Double] = Async {
      val a = Future {
        messages ::= (s"running future")
        Random.nextInt()
      }.toAsync.bind
      def of = Future { messages ::= (s"in another future, a was $a") }
      Sync { messages ::= ("just for show") }.run
      of.toAsync.bind
      val b = Sync { messages ::= ("runs later"); math.pow(2, 10) }.run
      Future { a % b }.toAsync.bind
    }

    messages ::= "runs first"
    res2.runToFuture(using global).map(res =>
      assertEquals(messages, List("runs later", messages(1), "just for show", "running future", "runs first"))
    )(using global)
  }

  test("resources and sequences") {

    class ExampleResource {
      @volatile var _isOpen: Boolean = true
      def isOpen: Boolean            = synchronized(_isOpen)
      def close() = synchronized:
        _isOpen = false
    }

    object ClosedControlException extends Exception("suppressed", null, false, false)

    val sources = List("long", "strings", "maybe", "should", "add", "some", "more?")

    val producer = Async[ExecutionContext] {
      val resource = Async(new ExampleResource()).bind

      val jobs = AtomicInteger(sources.length)

      val products = Async.fromCallback[String] {
        sources.foreach { item =>
          //println(s"producing $item")
          Future:
            // can only do operation (`reverse`) while resource is available
            if resource.isOpen
            then item.reverse
            else throw IllegalStateException("resource closed")
          .onComplete(Async.handler.complete)
        }
      }.transform { prod =>
        Async.fromCallback {
          //println(s"job done ${jobs.get()}")
          if jobs.decrementAndGet() == 0
          then
            //println(s"closing")
            resource.close()
            Async.handler.complete(prod)
            Async.handler.fail(ClosedControlException)
          else
            Async.handler.complete(prod)
        }
      }.bind

      products
    }

    val collector = Async[ExecutionContext].fromCallback {
      @volatile var collected: List[String] = Nil

      producer.run:
        case Success(v)                      => synchronized { collected = v :: collected }
        case Failure(ClosedControlException) => Async.handler.succeed(collected)
        case Failure(other)                  => Async.handler.fail(other)
    }

    val res: Async[ExecutionContext, Unit] = collector.map { res =>
      assertEquals(res.sorted, sources.map(_.reverse).sorted)
      ()
    }
    res.runToFuture(using global)
  }
}
