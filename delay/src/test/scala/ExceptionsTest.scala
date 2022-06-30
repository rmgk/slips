import de.rmgk.delay.*

class DelayTests extends munit.FunSuite {
  test("exception in async") {
    val as = Async { throw new IllegalStateException("test") }
    as.run {
      case Left(e)  => assert(e.getMessage == "test")
      case Right(_) => assert(false)
    }
  }

  test("exception nested") {
    val failed = Async { throw new IllegalStateException("test") }

    var count = 0

    val counting = Async {
      count += 1
      val success = Async { 100 }.await
      failed.await
      count += 1
    }

    assert(count == 0)

    counting.run {
      case Left(e)  => assert(e.getMessage == "test")
      case Right(_) => assert(false)
    }

    assert(count == 1)
  }

  test("exception nested2") {
    val failed = Async { throw new IllegalStateException("test") }

    var count = 0

    val counting = Async {
      count += 1
      val success = Async { 100 }.await
      count += 1
      val failure = {
        throw IllegalStateException("test2")
        Async(30)
      }.await
      count += 1
      failed.await
      count += 1
    }

    assert(count == 0)

    counting.run {
      case Left(e)  => assert(e.getMessage == "test2")
      case Right(_) => assert(false)
    }

    assert(count == 2)
  }

  test("close") {
    var messages: List[String] = Nil
    val m1                     = s"starting something"
    val m2                     = s"does not happen anymore"
    val m3                     = "resource"
    val me                     = "oh noes!"

    Async[String] {
      messages ::= m1
      throw IllegalStateException(me)
      messages ::= m2
    }.close { messages ::= summon[String] }
      .run {
        case Left(e) => assert(e.getMessage == me)
        case _       => assert(false)
      }(using m3)

    assert(messages == List(m3, m1))
  }
}