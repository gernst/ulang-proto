package ulang.calculus.linear

import ulang._
import ulang.syntax._

object Solver extends calculus.Solver {
  def canonize(expr: Expr): Expr = {
    val p = Polynomial(expr)
    val q = p.reduce // DO NOT NORMALIZE HERE!! it's not an equation
    val res = q.toExpr
    res
  }

  def solve(k: Int, c: Constraint): Expr = {
    val (y, op, r) = c.solve(k)
    Applys(op, List(y, r.toExpr))
  }

  def solve(expr: Expr, forbidden: List[Id]): (List[Expr], List[Id]) = {
    val c = Constraint(expr)

    c.p.unit match {
      case -1 if c.p.isConstant =>
        if (c.p.const == 0)
          (Nil, Nil)
        else
          throw calculus.Inconsistent

      case -1 =>
        val (y, k, cr) = Omega.reduce(c, forbidden)
        (List(solve(k, cr)), List(y))

      case k =>
        (List(solve(k, c)), Nil)
    }
  }
}