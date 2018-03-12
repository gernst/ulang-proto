package ulang.prove

import bk.Control
import bk.backtrack
import ulang.expr.App
import ulang.expr.Case
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
import ulang.shell

object rewrite {
  def bind(pat: Pat, arg: Expr, dyn: Binding, env: Binding): Binding = pat match {
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
      bind(pat, arg, dyn, env + (name -> arg))

    case UnApp(pfun, parg) =>
      arg match {
        case App(vfun, varg) =>
          bind(parg, varg, dyn, bind(pfun, vfun, dyn, env))
        case _ =>
          backtrack()
      }
  }

  def bind(pats: List[Pat], args: List[Expr], dyn: Binding, env: Binding): Binding = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, dyn, bind(pat, arg, dyn, env))

    case _ =>
      backtrack()
  }

  def apply(cs: Case, args: List[Expr], lex: Binding, dyn: Binding): Expr = cs match {
    case Case(pats, cond, body) =>
      val env = bind(pats, args, dyn, Binding.empty)
      val newlex = lex ++ env
      cond.map(rewrite(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => shell.error("not a boolean in pattern: " + res)
      }
      rewrite(body, newlex, dyn)
  }

  def apply(cases: List[Case], args: List[Expr], lex: Binding, dyn: Binding): Expr = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(id: Id, fun: Expr, args: List[Expr], dyn: Binding): Expr = fun match {
    case Lambda(cases) =>
      apply(cases, args, Binding.empty, dyn) or App(id, args)
    case _ =>
      App(id, args)
  }

  def rewrite(exprs: List[Expr], lex: Binding, dyn: Binding): List[Expr] = {
    exprs map (rewrite(_, lex, dyn))
  }

  def rewrite(expr: Expr, lex: Binding, dyn: Binding): Expr = expr match {
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

    case _ =>
      expr
  }
  
  def apply(phi: Expr, dyn: Binding): Expr = {
    rewrite(phi, Binding.empty, dyn)
  }
}