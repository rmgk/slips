import de.rmgk.delay.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

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

  test("retry counter") {
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

    val res = Await.result(makeFailing.runToFuture(using 4), Duration.Inf)
    assertEquals(res, "OK")

    val res2: Future[String] = Await.ready(makeFailing.runToFuture(using 2), Duration.Inf)

    assert(res2.value.get.isFailure)
  }

  test("provide flow") {
    val res: Async[Any, String] = Async[Any] {
      Async(4).bind
      Async.provided("provided") {
        summon
      }.bind
    }

    println(res.runToFuture)
  }

}
