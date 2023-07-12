import de.rmgk.delay.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
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
        Async.handler.succeed(1)
        Async.handler.succeed(2)
        var x = 2
        while
          x += 1
          x < 10
        do
          Async.handler.succeed(x)
        Async.handler.succeed(14)
      catch case CancelControl => println("done")

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
        Async.handler.succeed(1)
        Async.handler.succeed(2)
        var x = 2
        while
          x += 1
          x < 10
        do
          Async.handler.succeed(x)
        Async.handler.succeed(14)
      catch case CancelControl => println("done")

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

}
