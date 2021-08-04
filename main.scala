import expression.{Expression, Add, Mul, Variable, Constant, Log, Pow, Sin}
import differentiation.{Derivative}
import simplification.{Simplifier}

object Application {
  def main(args: Array[String]): Unit = {
    val y = new Mul(new Constant(10), new Add(new Constant(1), new Variable("x")))
    val dydx = Derivative.differentiate(y)
    val dydx_simplified = Simplifier.simplify(dydx)
    val dydx_s2 = Simplifier.simplify(dydx_simplified)
    println(s"y = $y")
    println(s"dy/dx = $dydx")
    println(s"simplified dy/dx = $dydx_simplified")
    println(s"simplified dy/dx = $dydx_s2")

    val z = new Pow(new Variable("x"), new Constant(2))
    val dzdx = Derivative.differentiate(z)
    println(s"z = $z")
    println(s"dz/dx = $dzdx")

    val s = new Sin(new Mul(Constant(2), new Variable("x")))
    val dsdx = Derivative.differentiate(s)
    val d2sdx2 = Derivative.differentiate(dsdx)
    val d3sdx3 = Derivative.differentiate(d2sdx2)
    val d4sdx4 = Derivative.differentiate(d3sdx3)
    val d4sdx4_simplify = Simplifier simplify(Simplifier simplify(Simplifier simplify(Simplifier simplify (Simplifier simplify (Simplifier simplify (Simplifier simplify d4sdx4))))))

    println(s"s = $s")
    println(s"ds/dx = $dsdx")
    println(s"ds/dx = $dsdx")
    println(s"d2s/dx2 = $d2sdx2")
    println(s"d3s/dx3 = $d3sdx3")
    println(s"d4s/dx4 = $d4sdx4")
    println(s"d4s/dx4 = $d4sdx4_simplify")
  }
}
