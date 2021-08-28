package integration
import expression.{Expression, Constant, Variable, Add, Sub, Mul, Div, Pow, Log, Neg, Sin, Cos, Tan}
import math.E

object Integral {
  def integrate(expression: Expression): Expression = expression match {
    case Constant(k) => Mul(Constant(k), Variable("x"))
    case Variable(x) => Div(Pow(Variable("x"), Constant(2)), Constant(2))
    case Add(left, right) => Add(integrate(left), integrate(right))
    case Sub(left, right) => Sub(integrate(left), integrate(right))
    case Neg(expr) => Neg(integrate(expr))
    case Pow(variable: Variable, Constant(k)) => Div(Pow(variable, Constant(k + 1)), Constant(k + 1))
    case Sin(x: Variable) => Neg(Cos(x))
    case Cos(x: Variable) => Sin(x)
    case Log(Constant(base), expression: Expression) => Div(Constant(1), Mul(expression, Log(Constant(E), Constant(base))))
  }
}
