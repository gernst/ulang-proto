package ulang.prove

import ulang.expr.Expr
import ulang.expr.builtin._

sealed trait Derivation
case class Goal(assume: List[Expr], assert: Expr) extends Derivation
case class Step(prems: List[Derivation], concl: Goal, rule: Rule) extends Derivation

object derive {
  def close(goal: Goal, rule: Rule): Derivation = {
    Step(List(), goal, rule)
  }

  def derive(expr: Expr, rule: Option[Rule]): Derivation = {
    derive(Goal(List(), expr), rule getOrElse Trivial)
  }

  def derive(goal: Goal, rule: Rule): Derivation = {
    val Goal(assume, assert) = goal

    rule match {
      case Trivial =>
        if (assert == True || (assume contains False))
          close(goal, rule)
        else goal
    }
  }
}