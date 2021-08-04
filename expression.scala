package expression

abstract class Expression {
  def toString: String
}

case class Constant(val value: Float) extends Expression { 
  override def toString: String = value.toString
}

case class Variable(val name: String) extends Expression {
  override def toString: String = name
}

case class Log(val base: Constant, body: Expression) extends Expression {
  override def toString: String = s"log_$base($body)"
}

case class Add(val left: Expression, val right: Expression) extends Expression {
  override def toString: String = s"($left + $right)"
}

case class Sub(val left: Expression, val right: Expression) extends Expression {
  override def toString: String = s"($left - $right)"
}

case class Mul(val left: Expression, val right: Expression) extends Expression {
  override def toString: String = s"($left * $right)"
}

case class Div(val left: Expression, val right: Expression) extends Expression {
  override def toString: String = s"($left / $right)"
}

case class Neg(val expr: Expression) extends Expression {
  override def toString: String = s"-$expr"
}

case class Pow(val base: Expression, val pow: Constant) extends Expression {
  override def toString: String = s"$base ^ $pow"
}

case class Exp(val base: Expression, val exponent: Expression) extends Expression {
  override def toString: String = s"$base ^ $exponent"
}

case class Sin(val expr: Expression) extends Expression {
  override def toString: String = s"sin($expr)"
}

case class Cos(val expr: Expression) extends Expression {
  override def toString: String = s"cos($expr)"
}

case class Tan(val expr: Expression) extends Expression {
  override def toString: String = s"tan($expr)"
}
