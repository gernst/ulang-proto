package ulang.core

import arse._
import ulang._

object Eval {
  import Matches._

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, args, lex, dyn) or { sys.error("cannot apply " + fun + " to " + args) }

    case _ =>
      sys.error("cannot apply " + fun + " to " + args)
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail

    case Case(pats, body) :: rest =>
      {
        val env = matches(pats, args, Env.empty)
        eval(body, lex ++ env, dyn)
      } or {
        apply(rest, args, lex, dyn)
      }
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map { eval(_, lex, dyn) }
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case Eq(lhs, rhs) =>
      val eq = equal(eval(lhs, lex, dyn), eval(rhs, lex, dyn))
      if (eq) True else False

    case LetIn(pat, arg, body) =>
      eval(body, matches(pat, eval(arg, lex, dyn), lex), dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case res => sys.error("not a boolean value: " + res)
      }

    case Apply(id @ Id(tag), args) if isTag(tag) =>
      Obj(tag, eval(args, lex, dyn))

    case Apply(fun, args) =>
      apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)

    case Match(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Lambda(cases) =>
      Closure(cases, lex)
  }
}