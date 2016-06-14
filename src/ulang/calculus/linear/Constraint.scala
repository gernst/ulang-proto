package ulang.calculus.linear

import Math.abs
import ulang._
import ulang.syntax._


// A linear constraint p op 0 
// where op in { <, <=, = }

case class Constraint(p: Polynomial, op: Id) {
  def isEq = (op == Id("=="))

  def toExpr = {
    val lhs = p.toExpr
    val rhs = Number(0)
    // Ap(op, List(lhs, rhs))
  }

  def solve(k: Int) = {
    val (y, q) = p.solve(k)
    (y, op, q)
  }

  def normalize = {
    val g = gcd(p.coefficients)
    if (isEq && p.const % g != 0) throw calculus.Inconsistent
    Constraint(p / g, op)
  }
}

object Constraint {
  def apply(expr: Expr): Constraint = expr match {
    case Eq(lhs, rhs) =>
      val pl = Polynomial(lhs)
      val pr = Polynomial(rhs)
      val p = (pl - pr).reduce.normalize
      Constraint(p, Eq.op)
  }
}