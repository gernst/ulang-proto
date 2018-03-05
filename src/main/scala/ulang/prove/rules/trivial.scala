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
  def apply(goal: Goal, rule: Rule): Derivation = goal match {
    case Goal(_, True) =>
      goal close rule
    case Goal(_, App(Id("="), List(x, y))) if x == y =>
      goal close rule
    case Goal(ant, suc) if (ant contains False) || (ant contains suc) =>
      goal close rule
    case _ =>
      goal
  }
}