package ulang.prove

import ulang.expr.builtin.False
import ulang.expr.builtin.True
import ulang.expr.App
import ulang.expr.Id

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