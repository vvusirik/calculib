package simplification
import expression.{Expression, Constant, Variable, Add, Sub, Mul, Div, Pow, Log, Neg, Sin, Cos, Tan}

object Simplifier {
  private def reduce(expression: Expression): Expression = expression match {
    case Neg(Neg(expr: Expression)) => expr
    case Neg(Constant(k)) => Constant(-k)
    case Add(expr: Expression, Constant(0)) => expr
    case Add(Constant(0), expr: Expression) => expr
    case Mul(expr: Expression, Constant(1)) => expr
    case Mul(Constant(1), expr: Expression) => expr
    case Mul(_, Constant(0)) => Constant(0)
    case Mul(Constant(0), _) => Constant(0)
    case Mul(Constant(k1), Mul(Constant(k2), expr: Expression)) => Mul(Constant(k1 * k2), expr)
    case Mul(Constant(k1), Mul(expr: Expression, Constant(k2))) => Mul(Constant(k1 * k2), expr)
    case Mul(Constant(k), Neg(expr)) => Mul(Constant(-k), expr)
    case Mul(Neg(expr), Constant(k)) => Mul(Constant(-k), expr)
    case Div(expr: Expression, Constant(1)) => expr
    case Add(Constant(k1), Constant(k2)) => Constant(k1 + k2)
    case Sub(Constant(k1), Constant(k2)) => Constant(k1 - k2)
    case Mul(Constant(k1), Constant(k2)) => Constant(k1 * k2)
    case Div(Constant(k1), Constant(k2)) => Constant(k1 / k2)
    case Pow(expr: Expression, Constant(0)) => Constant(0)
    case Pow(expr: Expression, Constant(1)) => expr
    case Log(Constant(k1), Constant(1)) => Constant(0)
    case Log(Constant(k1), Constant(k2)) if k1 == k2 => Constant(1)
    case _ => expression
  }

  def simplify(expression: Expression): Expression =  expression match {
    case Add(left: Expression, right: Expression) => reduce(Add(simplify(left), simplify(right)))
    case Sub(left: Expression, right: Expression) => reduce(Sub(simplify(left), simplify(right)))
    case Mul(left: Expression, right: Expression) => reduce(Mul(simplify(left), simplify(right)))
    case Div(left: Expression, right: Expression) => reduce(Div(simplify(left), simplify(right)))
    case Neg(expr: Expression) => reduce(Neg(simplify(expr)))
    case Sin(expr: Expression) => reduce(Sin(simplify(expr)))
    case Cos(expr: Expression) => reduce(Cos(simplify(expr)))
    case Tan(expr: Expression) => reduce(Tan(simplify(expr)))
    case _ => reduce(expression)
  }
}
