
import de.rmgk.delay.*

class ExceptionsTest extends munit.FunSuite {
  test("in async") {
    val as = Async { throw new IllegalStateException("test")}
    as.run{
      case Left(e) => assert(e.getMessage == "test")
      case Right(_) => assert(false)
    }
  }

  test("nested") {
    val failed = Async { throw new IllegalStateException("test")}

    var count = 0

    val counting = Async {
      count += 1
      val success = Async{ 100 }.await
      failed.await
      count += 1
    }

    assert(count == 0)

    counting.run{
      case Left(e) => assert(e.getMessage == "test")
      case Right(_) => assert(false)
    }

    assert(count == 1)
  }

  test("nested2") {
    val failed = Async { throw new IllegalStateException("test")}

    var count = 0

    val counting = Async {
      count += 1
      val success = Async{ 100 }.await
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

    counting.run{
      case Left(e) => assert(e.getMessage == "test2")
      case Right(_) => assert(false)
    }

    assert(count == 2)
  }
}
