package ulang.prove

import ulang.Pretty
import ulang.expr.App
import ulang.expr.Expr
import ulang.expr.Id
import ulang.expr.builtin.False
import ulang.expr.builtin.True
import ulang.expr.builtin.==>
import ulang.expr.builtin.and
import ulang.expr.Pat
import bk._

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
  def trivial(goal: Goal, rule: Rule): Derivation = goal match {
    case Goal(_, True) =>
      goal close rule
    case Goal(_, App(Id("="), List(x, y))) if x == y =>
      goal close rule
    case Goal(ant, suc) if (ant contains False) || (ant contains suc) =>
      goal close rule
    case _ =>
      goal
  }
  
  def cut(phi: Expr, goal: Goal, rule: Rule): Derivation = {
    val prem1 = assert(phi, goal)
    val prem2 = assume(phi, goal)
    Step(List(prem1, prem2), goal, rule)
  }

  def induction(expr: Expr, cases: List[(Pat, Rule)], goal: Goal, rule: Rule): Derivation = {
    val Goal(ant, suc) = goal
    if (!(ant contains expr)) backtrack()
    ???
  }

  def assume(phi: Expr, goal: Goal): Goal = phi match {
    case True =>
      goal
    case phi and psi =>
      assume(phi, assume(psi, goal))
    case _ =>
      val Goal(ant, suc) = goal
      Goal(phi :: ant, suc)
  }

  def assert(phi: Expr, goal: Goal): Goal = phi match {
    case phi ==> psi =>
      assume(phi, assert(psi, goal))
    case _ =>
      val Goal(ant, suc) = goal
      if (phi == False) ulang.shell.warning("asserting false")
      if (suc != True) ulang.shell.warning("weaken ... ==> " + suc)
      Goal(ant, phi)
  }

  def derive(expr: Expr, rule: Option[Rule], dyn: Binding): Derivation = {
    val phi = rewrite(expr, dyn)
    val goal = assert(phi, Goal.empty)
    derive(goal, rule getOrElse Trivial, dyn)
  }

  def derive(goal: Goal, rule: Rule, dyn: Binding): Derivation = {
    rule match {
      case Trivial =>
        trivial(goal, rule)
      case Cut(phi) =>
        cut(phi, goal, rule)
      case Induction(expr, cases) =>
        induction(expr, cases, goal, rule)
    }
  }
}