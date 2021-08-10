import org.scalatest.funsuite._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import differentiation.Derivative.differentiate
import expression.{Expression, Add, Mul, Variable, Constant, Log, Pow, Sin}

class DifferentiationTest extends AnyFunSuite {
  test("Constant derivative is 0") {
    assert(differentiate(Constant(1)) == Constant(0))
  }

  test("Variable derivative is 1") {
    assert(differentiate(Variable("x")) == Constant(1))
  }

}

//class DifferentiationSpec extends Properties("Differentiation") {
  //property("Sum of derivatives is derivative of sum") = forAll { (a: Expression, b: Expression) =>
    //Add(differentiate(a), differentiate(b)) == differentiate(Add(a, b))
  //}
}
