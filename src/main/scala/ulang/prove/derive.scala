package ulang.prove

import bk.backtrack
import bk.Control
import ulang.Pretty
import ulang.expr.App
import ulang.expr.Expr
import ulang.expr.Free
import ulang.expr.Pat
import ulang.expr.builtin.False
import ulang.expr.builtin.True
import ulang.expr.builtin.and
import ulang.expr.builtin.eq
import ulang.expr.builtin.==>
import ulang.expr.unify
import ulang.expr.builtin
import ulang.expr.Apps

sealed trait Derivation extends Pretty

case class Goal(eqs: List[(Expr, Expr)], ant: List[Expr], suc: Expr) extends Derivation {
  def close(rule: Rule): Derivation = {
    Step(List(), this, rule)
  }
}

object Goal {
  val empty = init(True)
  def init(phi: Expr) = Goal(List(), List(), phi)

  def normalized(suc: Expr): Goal = {
    import derive.assert
    assert(suc, empty)
  }

  def normalized(ant: List[Expr], suc: Expr): Goal = {
    import derive.assume
    import derive.assert
    assume(ant, assert(suc, empty))
  }
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
    case Goal(_, _, x eq y) if x == y =>
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

  def induction_prems(expr: Expr, goal: Goal, constrs: List[Goal], tried: List[Case], cases: List[Case], dyn: Env, ind: Ind): List[Derivation] = (constrs, cases) match {
    case (Nil, _) =>
      if (cases.isEmpty)
        ulang.warning("extra cases " + cases)
      Nil

    case (goal :: constrs, Nil) =>
      // prove by trivial, however, need to rename a lot?
      ???

    case (Goal(_, prems, concl) :: constrs, (cs @ Case(pat, rule)) :: cases) =>
      val Apps(fun: Free, args1) = expr
      val Apps(`fun`, args2) = concl

      {
        val lex = rewrite.bind(pat, concl, Env.empty, dyn)

        val eqs = (args1, args2).zipped.map {
          case (arg1, arg2) => builtin.eq(arg1, arg2)
        }

        // TODO: inductive hypotheses

        val prem = derive(assume(eqs, goal), rule, dyn, ind)
        prem :: induction_prems(expr, goal, constrs, Nil, tried.reverse ++ cases, dyn, ind)
      } or {
        induction_prems(expr, goal, constrs, cs :: tried, cases, dyn, ind)
      }
  }

  def induction(expr: Expr, cases: List[Case], goal: Goal, rule: Rule, dyn: Env, ind: Ind): Derivation = {
    val Goal(_, ant, suc) = goal

    if (!(ant contains expr))
      backtrack()

    val App(fun: Free, args1) = expr

    val List(constrs) = ind.collect {
      case (pat, constrs) if rewrite.matches(pat, expr, dyn) =>
        constrs
    }

    val premlists = for (Goal(_, prems, concl) <- constrs) yield {
      /*
      val cs = cases

        // Case(pat, rule) <- cases)

      val App(`fun`, args2) = concl

      {
        val lex = rewrite.bind(pat, concl, Env.empty, dyn)

        val eqs = (args1, args2).zipped.map {
          case (arg1, arg2) => builtin.eq(arg1, arg2)
        }

        // TODO: inductive hypotheses

        derive(assume(eqs, goal), rule, dyn, ind)
      } or {
        ???
      }
      */
      Nil
    }

    Step(premlists.flatten, goal, rule)
  }

  def equal(lhs: Expr, rhs: Expr, goal: Goal): Goal = (lhs, rhs) match {
    case _ if lhs == rhs =>
      goal
    case (Apps(fun1: Free, args1), Apps(fun2: Free, args2)) if fun1 == fun2 =>
      (args1, args2).zipped.foldRight(goal) {
        case ((arg1, arg2), goal) => equal(arg1, arg2, goal)
      }
    case _ =>
      val Goal(eqs, ant, suc) = goal
      Goal((lhs, rhs) :: eqs, ant, suc)
  }

  def assume(phi: Expr, goal: Goal): Goal = phi match {
    case True =>
      goal
    case phi and psi =>
      assume(phi, assume(psi, goal))
    case lhs eq rhs =>
      equal(lhs, rhs, goal)
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

  def process(eqs: List[(Expr, Expr)], ant: List[Expr], suc: Expr, dyn: Env): Goal = ant match {
    case Nil =>
      val Goal(_, newant, newsuc) = Goal.normalized(rewrite(suc, dyn))
      Goal(eqs, newant, newsuc)
    case phi :: ant =>
      assume(rewrite(phi, dyn), process(eqs, ant, suc, dyn))
  }

  def process_plus(eqs: List[(Expr, Expr)], ant: List[Expr], suc: Expr, lex: Env, dyn: Env): Goal = ant match {
    case Nil =>
      val Goal(_, newant, newsuc) = Goal.normalized(rewrite.rewrite(suc, lex, dyn))
      Goal(eqs, newant, newsuc)
    case phi :: ant =>
      assume(rewrite.rewrite(phi, lex, dyn), process_plus(eqs, ant, suc, lex: Env, dyn))
  }

  def derive(phi: Expr, rule: Option[Rule], dyn: Env, ind: Ind): Derivation = {
    val goal = Goal.normalized(phi)
    derive(goal, rule getOrElse Trivial, dyn, ind) or goal
  }

  def derive(goal: Goal, rule: Rule, dyn: Env, ind: Ind): Derivation = {
    rule match {
      case Sorry =>
        Step(List(goal), goal, rule)
      case Trivial =>
        val Goal(eqs, ant, suc) = goal
        val lex = eqs collect { case (Free(name), rhs) => (name, rhs) }
        trivial(process_plus(eqs, ant, suc, lex.toMap, dyn), rule)
      case Cut(phi) =>
        cut(phi, goal, rule)
      case Induction(expr, cases) =>
        induction(expr, cases, goal, rule, dyn, ind)
    }
  }
}