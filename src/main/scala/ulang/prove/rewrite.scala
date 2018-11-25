package ulang.prove

import bk.Backtrack
import bk.Control
import bk.NoStackTrace
import bk.backtrack
import ulang.expr.App
import ulang.expr.Expr
import ulang.expr.Lambda
import ulang.expr.Pat
import ulang.expr.Tag
import ulang.expr.UnApp
import ulang.expr.Var
import ulang.expr.Wildcard
import ulang.expr.Apps
import ulang.expr.UnApps

object rewrite {
  case object Postponed extends Throwable with NoStackTrace

  def postpone() = {
    throw Postponed
  }

  def matches(pat: Pat, arg: Expr) = {
    try {
      println("try match " + pat + " with " + arg)
      val env = bind(pat, arg, Env.empty)
      println("success: " + env)
      true
    } catch {
      case Backtrack | Postponed =>
        false
    }
  }

  def equal(e1: Expr, e2: Expr): Boolean = (e1, e2) match {
    case _ if e1 == e2 =>
      true
    case (Apps(tag1: Tag, args1), Apps(tag2: Tag, args2)) =>
      false
    case _ =>
      postpone()
  }

  def equal(p1: Pat, e2: Expr): Boolean = (p1, e2) match {
    case _ if p1 == e2 =>
      true
    case (UnApps(tag1: Tag, args1), Apps(tag2: Tag, args2)) =>
      false
    case _ =>
      postpone()
  }

  def bind(pat: Pat, arg: Expr, env: Env): Env = pat match {
    case Wildcard =>
      env

    case x: Var if env contains x =>
      if (equal(env(x), arg)) env
      else backtrack()

    case x: Var =>
      println("bind " + x + " to " + arg + " in " + env)
      env + (x -> arg)

    case tag: Tag =>
      if (equal(tag: Pat, arg)) env
      else backtrack()

    case UnApp(pfun, parg) =>
      arg match {
        case App(vfun, varg) =>
          bind(parg, varg, bind(pfun, vfun, env))
        // case pfun has a tag in function position and this is also a tag => backtrack()
        case _ =>
          if (!equal(pat, arg)) backtrack()
          else ???
      }

    case _ =>
      postpone()
  }

  def apply(cs: ulang.expr.Case, arg: Expr, dyn: Env): Expr = cs match {
    case ulang.expr.Case(pat, body) =>
      val lex: Env = ??? // needs to start with some context!
      rewrite(body, bind(pat, arg, lex), dyn)
  }

  def apply(cases: List[ulang.expr.Case], arg: Expr, dyn: Env): Expr = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      // XXX: if the first case does not MISMATCH, keep it and abort
      apply(cs, arg, dyn) or apply(rest, arg, dyn)
  }

  def apply(fun: Expr, body: Expr, arg: Expr, dyn: Env): Expr = {
    val res = body match {
      case Lambda(cases) =>
        try {
          apply(cases, arg, dyn) or App(fun, arg)
        } catch {
          case Postponed => App(fun, arg)
        }
      case _ =>
        App(fun, arg)
    }
    res
  }

  def rewrite(cs: ulang.expr.Case, lex: Env, dyn: Env): ulang.expr.Case = cs match {
    case ulang.expr.Case(pat, body) =>
      /* for a variable x in pat:
       * if it is in the domain of lex then this imposes an equality constraint on the argument passed to this case == lex(x)
       * otherwise, potentially rename it to avoid capturing variables in the range of lex
       */   
      ???
  }

  def rewrite(exprs: List[Expr], lex: Env, dyn: Env): List[Expr] = {
    exprs map (rewrite(_, lex, dyn))
  }

  def rewrite(expr: Expr, lex: Env, dyn: Env): Expr = {
    val res = expr match {
      case x: Var if lex contains x =>
        lex(x)

      case x: Var if dyn contains x =>
        dyn(x)

      case App(fun, arg) =>
        apply(fun, rewrite(fun, lex, dyn), rewrite(arg, lex, dyn), dyn)

      case Lambda(cases) =>
        Lambda(cases map (rewrite(_, lex, dyn)))

      case _ =>
        expr
    }
    if (res != expr) println("rewrite " + expr + " = " + res)
    res
  }

  def apply(phi: Expr, dyn: Env): Expr = {
    rewrite(phi, Env.empty, dyn)
  }
}