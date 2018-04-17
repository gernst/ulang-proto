package ulang.prove

import bk.Control
import bk.backtrack
import ulang.expr.App
import ulang.expr.Expr
import ulang.expr.Id
import ulang.expr.IfThenElse
import ulang.expr.Lambda
import ulang.expr.Lit
import ulang.expr.Pat
import ulang.expr.SubPat
import ulang.expr.Tag
import ulang.expr.UnApp
import ulang.expr.Wildcard
import ulang.expr.builtin

object rewrite {
  def matches(pat: Pat, arg: Expr, dyn: Env) = {
    { bind(pat, arg, Env.empty, dyn); true } or { false }
  }

  def bind(pat: Pat, arg: Expr, env: Env, dyn: Env): Env = pat match {
    case Wildcard =>
      env

    case Lit(any) =>
      if (any == arg) env
      else backtrack()

    case id @ Tag(_) =>
      if (id == arg) env
      else backtrack()

    case Id(name) =>
      (env get name) match {
        case Some(that) if that == arg => env
        case None => env + (name -> arg)
        case _ => backtrack()
      }

    case SubPat(name, pat) =>
      bind(pat, arg, env + (name -> arg), dyn)

    case UnApp(pfun, parg) =>
      arg match {
        case App(vfun, varg) =>
          bind(parg, varg, bind(pfun, vfun, env, dyn), dyn)
        case _ =>
          backtrack()
      }
  }

  def bind(pats: List[Pat], args: List[Expr], env: Env, dyn: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, bind(pat, arg, env, dyn), dyn)

    case _ =>
      backtrack()
  }

  def apply(cs: ulang.expr.Case, args: List[Expr], lex: Env, dyn: Env): Expr = cs match {
    case ulang.expr.Case(pats, cond, body) =>
      val env = bind(pats, args, dyn, Env.empty)
      val newlex = lex ++ env
      cond.map(rewrite(_, newlex, dyn)).foreach {
        case builtin.True =>
        case _ => backtrack()
      }
      rewrite(body, newlex, dyn)
  }

  def apply(cases: List[ulang.expr.Case], args: List[Expr], lex: Env, dyn: Env): Expr = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(id: Id, fun: Expr, args: List[Expr], dyn: Env): Expr = {
    val res = fun match {
      case Lambda(cases) =>
        apply(cases, args, Env.empty, dyn) or App(id, args)
      case _ =>
        App(id, args)
    }
    res
  }

  def rewrite(exprs: List[Expr], lex: Env, dyn: Env): List[Expr] = {
    exprs map (rewrite(_, lex, dyn))
  }

  def rewrite(expr: Expr, lex: Env, dyn: Env): Expr = {
    val res = expr match {
      case Id(name) if lex contains name =>
        lex(name)

      case Id(name) if dyn contains name =>
        dyn(name)

      case IfThenElse(test, arg1, arg2) =>
        lazy val newarg1 = rewrite(arg1, lex, dyn)
        lazy val newarg2 = rewrite(arg2, lex, dyn)

        rewrite(test, lex, dyn) match {
          case builtin.True => newarg1
          case builtin.False => newarg2
          case newtest => IfThenElse(newtest, newarg1, newarg2)
        }

      case App(fun: Id, args) =>
        apply(fun, rewrite(fun, lex, dyn), rewrite(args, lex, dyn), dyn)
        
      case App(tag: Tag, args) =>
        App(tag, rewrite(args, lex, dyn))

      case _ =>
        expr
    }
    // if(res != expr) println("rewrite " + expr + " = " + res)
    res
  }

  def apply(phi: Expr, dyn: Env): Expr = {
    rewrite(phi, Env.empty, dyn)
  }
}