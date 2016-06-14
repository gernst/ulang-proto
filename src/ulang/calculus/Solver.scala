package ulang.calculus

import ulang._

object Inconsistent extends Exception

trait Solver {
  // canonize expression by computing a normal form wrt. this theory
  def canonize(expr: Expr): Expr
  
  // solve an equation for a variable (non-theory expression)
  // returning potentially new identifiers as well
  def solve(eq: Expr, vars: List[Id]): (List[Expr], List[Id])
}