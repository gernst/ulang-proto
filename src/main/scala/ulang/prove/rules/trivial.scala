package ulang.prove.rules

import ulang.expr.builtin.False
import ulang.expr.builtin.True
import ulang.prove.Goal
import ulang.prove.Rule
import ulang.expr.Expr
import ulang.expr.App
import ulang.expr.Id
import ulang.prove.Derivation

object trivial {
  def apply(goal: Goal, rule: Rule): Derivation = {
    val Goal(ant, suc) = goal

    if (suc == True || (ant contains False) || (ant contains suc))
      goal close rule
    else goal
  }
}