import org.scalatest.funsuite._
import expression.{Expression}

class HelloSpec extends AnyFunSuite {
  test("Hello should start with H") {
    assert("hello".startsWith("H"))
  }
}
