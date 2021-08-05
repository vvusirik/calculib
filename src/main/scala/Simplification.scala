package simplification
import expression.{Expression, Constant, Variable, Add, Sub, Mul, Div, Pow, Log, Neg, Sin, Cos, Tan}

object Simplifier {
  def simplify(expression: Expression): Expression = expression match {
    case Add(expr: Expression, Constant(0)) => simplify(expr)
    case Add(Constant(0), expr: Expression) => simplify(expr)
    case Mul(expr: Expression, Constant(1)) => simplify(expr)
    case Mul(Constant(1), expr: Expression) => simplify(expr)
    case Mul(_, Constant(0)) => Constant(0)
    case Mul(Constant(0), _) => Constant(0)
    case Div(expr: Expression, Constant(1)) => simplify(expr)
    case Add(Constant(k1), Constant(k2)) => Constant(k1 + k2)
    case Sub(Constant(k1), Constant(k2)) => Constant(k1 - k2)
    case Mul(Constant(k1), Constant(k2)) => Constant(k1 * k2)
    case Div(Constant(k1), Constant(k2)) => Constant(k1 / k2)
    case Add(left: Expression, right: Expression) => Add(simplify(left), simplify(right))
    case Sub(left: Expression, right: Expression) => Sub(simplify(left), simplify(right))
    case Mul(left: Expression, right: Expression) => Mul(simplify(left), simplify(right))
    case Div(left: Expression, right: Expression) => Div(simplify(left), simplify(right))
    case Neg(expr: Expression) => Neg(simplify(expr))
    case Sin(expr: Expression) => Sin(simplify(expr))
    case Cos(expr: Expression) => Cos(simplify(expr))
    case Tan(expr: Expression) => Tan(simplify(expr))
    case _ => expression
  }
}
