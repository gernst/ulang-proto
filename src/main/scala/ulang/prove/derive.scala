package ulang.prove

import bk.backtrack
import bk.Control
import ulang.Pretty
import ulang.expr.App
import ulang.expr.Expr
import ulang.expr.Id
import ulang.expr.Pat
import ulang.expr.builtin.False
import ulang.expr.builtin.True
import ulang.expr.builtin.and
import ulang.expr.builtin.eq
import ulang.expr.builtin.==>
import ulang.expr.unify
import ulang.expr.builtin

sealed trait Derivation extends Pretty

case class Goal(eqs: List[(Expr, Expr)], ant: List[Expr], suc: Expr) extends Derivation {
  def close(rule: Rule): Derivation = {
    Step(List(), this, rule)
  }
}

object Goal {
  val empty = init(True)
  def init(phi: Expr) = Goal(List(), List(), phi)
}

case class Step(prems: List[Derivation], concl: Goal, rule: Rule) extends Derivation

object Step {
  def normalized(prems: List[Goal], concl: Goal, rule: Rule) = {
    val new_prems = prems map {
      case prem @ Goal(eqs, ant, suc) =>
        val new_prem = derive.assume(ant, derive.assert(suc, Goal(eqs, List(), True)))
        derive.trivial(new_prem, Trivial)
    }
    Step(new_prems, concl, rule)
  }
}

object derive {
  def congruence(goal: Goal) = {
    val Goal(eqs, ant, suc) = goal
    val cong = new congruence()

    for ((lhs, rhs) <- eqs) {
      cong += (lhs, rhs)
    }

  }

  def trivial(goal: Goal, rule: Rule): Derivation = goal match {
    case Goal(_, _, True) =>
      goal close rule
    case Goal(_, _, App(Id("="), List(x, y))) if x == y =>
      goal close rule
    case Goal(_, ant, suc) if (ant contains False) || (ant contains suc) =>
      goal close rule
    case _ =>
      goal
  }

  def cut(phi: Expr, goal: Goal, rule: Rule): Derivation = {
    val prem1 = assert(phi, goal)
    val prem2 = assume(phi, goal)
    Step.normalized(List(prem1, prem2), goal, rule)
  }

  def induction(expr: Expr, cases: List[Case], goal: Goal, rule: Rule, dyn: Env, ind: Ind): Derivation = {
    val Goal(_, ant, suc) = goal

    if (!(ant contains expr))
      backtrack()

    val App(fun: Id, args1) = expr

    val List(constrs) = ind.collect {
      case (pat, constrs) if rewrite.matches(pat, expr, dyn) =>
        constrs
    }

    val prems = for ((Goal(_, prems, concl), Case(pat, rule)) <- (constrs, cases).zipped) yield {
      val App(`fun`, args2) = concl
      val eqs = (args1, args2).zipped.map {
        case (arg1, arg2) => builtin.eq(arg1, arg2)
      }

      val lex = rewrite.bind(pat, concl, Env.empty, dyn)
      // for inductive hypotheses

      derive(assume(eqs, goal), rule, dyn, ind)
    }

    Step(prems.toList, goal, rule)
  }

  def assume(phi: Expr, goal: Goal): Goal = phi match {
    case True =>
      goal
    case phi and psi =>
      assume(phi, assume(psi, goal))
    case lhs eq rhs =>
      val Goal(eqs, ant, suc) = goal
      Goal((lhs, rhs) :: eqs, ant, suc)
    case _ =>
      val Goal(eqs, ant, suc) = goal
      Goal(eqs, phi :: ant, suc)
  }

  def assume(phis: List[Expr], goal: Goal): Goal = {
    phis.foldRight(goal)(assume)
  }

  def assert(phi: Expr, goal: Goal): Goal = phi match {
    case phi ==> psi =>
      assume(phi, assert(psi, goal))
    case _ =>
      val Goal(eqs, ant, suc) = goal
      if (phi == False) ulang.warning("asserting false")
      if (suc != True) ulang.warning("weaken ... ==> " + suc)
      Goal(eqs, ant, phi)
  }

  def assert(phis: List[Expr], goal: Goal): Goal = {
    phis.foldRight(goal)(assert)
  }

  def derive(phi: Expr, rule: Option[Rule], dyn: Env, ind: Ind): Derivation = {
    val goal = assert(phi, Goal.empty)
    // val goal = Goal(List(), List(), phi)
    derive(goal, rule getOrElse Trivial, dyn, ind)
  }

  def derive(goal: Goal, rule: Rule, dyn: Env, ind: Ind): Derivation = {
    rule match {
      case Trivial =>
        trivial(goal, rule)
      case Cut(phi) =>
        cut(phi, goal, rule)
      case Induction(expr, cases) =>
        induction(expr, cases, goal, rule, dyn, ind)
    }
  }
}