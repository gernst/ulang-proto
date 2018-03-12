package ulang.prove

import bk.backtrack
import ulang.expr.Expr
import ulang.expr.Pat

object induction {
  def apply(expr: Expr, cases: List[(Pat, Rule)], goal: Goal, rule: Rule): Derivation = {
    val Goal(ant, suc) = goal
    if (!(ant contains expr)) backtrack()
    ???
  }
}