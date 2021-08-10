import expression.{Expression, Add, Mul, Variable, Constant, Log, Pow, Sin}
import differentiation.Derivative.differentiate
import simplification.Simplifier.simplify
import math.E

object Application {
  def main(args: Array[String]): Unit = {
    def derive = differentiate _ andThen simplify _
    val y = Mul(Constant(10), Add(Constant(1), Variable("x")))
    val dydx = derive(y)

    println(s"y = $y")
    println(s"dy/dx = $dydx")
    println()

    val z = Pow(Variable("x"), Constant(2))
    val dzdx = derive(z)
    println(s"z = $z")
    println(s"dz/dx = $dzdx")
    println()

    val s = Sin(Mul(Variable("x"), Constant(2)))
    val dsdx = derive(s)
    val d2sdx2 = derive(dsdx)
    val d3sdx3 = derive(d2sdx2)
    val d4sdx4 = derive(d3sdx3)

    println(s"s = $s")
    println(s"ds/dx = $dsdx")
    println(s"d2s/dx2 = $d2sdx2")
    println(s"d3s/dx3 = $d3sdx3")
    println(s"d4s/dx4 = $d4sdx4")

    val n = Log(Constant(E), Variable("x"))
    val dndx = derive(n)
    println(s"dn/dx = $dndx")

    val l = Log(Constant(10), Variable("x"))
    val dldx = derive(l)
    println(s"dl/dx = $dldx")

  }
}
