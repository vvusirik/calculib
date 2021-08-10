package differentiation
import expression.{Expression, Constant, Variable, Add, Sub, Mul, Div, Pow, Log, Neg, Sin, Cos, Tan}
import math.E

object Derivative {
  def differentiate(expression: Expression): Expression = expression match {
    case Constant(k) => Constant(0)
    case Variable(x) => Constant(1)
    case Add(left, right) => Add(differentiate(left), differentiate(right))
    case Sub(left, right) => Sub(differentiate(left), differentiate(right))
    case Mul(left, right) => Add(Mul(left, differentiate(right)), Mul(differentiate(left), right))
    case Neg(expr) => Neg(differentiate(expr))
    case Pow(expression: Expression, power) => Mul(differentiate(expression), Mul(power, Pow(expression, Constant(power.value -1))))
    case Sin(expr: Expression) => Mul(differentiate(expr), Cos(expr))
    case Cos(expr: Expression) => Mul(differentiate(expr), Neg(Sin(expr)))
    case Tan(expr: Expression) => Mul(differentiate(expr), Div(Constant(1), Pow(Tan(expr), Constant(2))))
    case Log(Constant(base), expression: Expression) => Div(Constant(1), Mul(expression, Log(Constant(E), Constant(base))))
  }
}
