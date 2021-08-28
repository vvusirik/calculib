import org.scalatest.funsuite._
import org.scalacheck._
import Prop._
import differentiation.Derivative.differentiate
import expression.{Expression, Add, Sub, Mul, Div, Variable, Constant, Log, Pow, Sin}

class DifferentiationTest extends AnyFunSuite {
  test("Constant derivative is 0") {
    assert(differentiate(Constant(1)) == Constant(0))
  }

  test("Variable derivative is 1") {
    assert(differentiate(Variable("x")) == Constant(1))
  }

}

class DifferentiationProperties extends Properties("Differentiation") {
  def constants: Gen[Constant] = Gen.choose(-100, 100).map(Constant(_))
  def variables: Gen[Variable] = Variable("x")

  def addExprs: Gen[Add] = for {
    left <- expressions
    right <- expressions
  } yield Add(left, right)
  def subExprs: Gen[Sub] = for {
    left <- expressions
    right <- expressions
  } yield Sub(left, right)
  def mulExprs: Gen[Mul] = for {
    left <- expressions
    right <- expressions
  } yield Mul(left, right)
  def divExprs: Gen[Div] = for {
    left <- expressions
    right <- expressions
  } yield Div(left, right)

  def expressions: Gen[Expression] = Gen.lzy(Gen.oneOf(constants, variables, addExprs, subExprs))

  implicit lazy val arbConstant: Arbitrary[Constant] = Arbitrary(constants)
  implicit lazy val arbVariable: Arbitrary[Variable] = Arbitrary(variables)
  implicit lazy val arbExpression: Arbitrary[Expression] = Arbitrary(expressions)

  property("Derivative of constant is 0") = forAll {(c: Constant) =>
    differentiate(c) == Constant(0)
  }

  property("Derivative of variable is 1") = forAll {(v: Variable) =>
    differentiate(v) == Constant(1)
  }

  property("Sum of derivatives is derivative of sum") = forAll { (a: Expression, b: Expression) =>
    Add(differentiate(a), differentiate(b)) == differentiate(Add(a, b))
  }

  property("Difference of derivatives is derivative of difference") = forAll { (a: Expression, b: Expression) =>
    Sub(differentiate(a), differentiate(b)) == differentiate(Sub(a, b))
  }
}
