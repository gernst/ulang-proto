package ulang.prove

import ulang.expr.Expr
import ulang.expr.builtin._
import ulang.Pretty
import ulang.expr.Id
import ulang.expr.App

sealed trait Derivation extends Pretty

case class Goal(ant: List[Expr], suc: Expr) extends Derivation {
  def close(rule: Rule): Derivation = {
    Step(List(), this, rule)
  }
}

object Goal {
  val empty = Goal(List(), True)
}

case class Step(prems: List[Derivation], concl: Goal, rule: Rule) extends Derivation

object derive {
  def assume(phi: Expr, goal: Goal): Goal = phi match {
    case True =>
      goal
    case App(Id("and"), List(phi, psi)) =>
      assume(phi, assume(psi, goal))
    case _ =>
      val Goal(ant, suc) = goal
      Goal(phi :: ant, suc)
  }

  def assert(phi: Expr, goal: Goal): Goal = phi match {
    case False =>
      goal
    case App(Id("==>"), List(phi, psi)) =>
      assume(phi, assert(psi, goal))
    case _ if goal.suc == True =>
      val Goal(ant, _) = goal
      Goal(ant, phi)
  }

  def derive(expr: Expr, rule: Option[Rule]): Derivation = {
    val goal = assert(expr, Goal.empty)
    derive(goal, rule getOrElse Trivial)
  }

  def derive(goal: Goal, rule: Rule): Derivation = {
    rule match {
      case Trivial =>
        rules.trivial(goal, rule)
    }
  }
}