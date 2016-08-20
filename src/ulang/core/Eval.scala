package ulang.core

import arse._
import ulang._

object Eval {
  var indent = "eval   "

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, args, lex, dyn) or { sys.error("cannot apply " + fun + " to " + args) }

    case _ =>
      sys.error("cannot apply " + fun + " to " + args)
  }

  def matches(pat: Pat, arg: Val, env: Env): Env = {
    val res = _matches(pat, arg, env)
    // println(indent + pat + " | " + arg + " ~> " + res + " <~ " + env)
    res
  }

  def matches(pats: List[Pat], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      matches(pats, args, matches(pat, arg, env))

    case _ =>
      fail
  }

  def _matches(pat: Pat, arg: Val, env: Env): Env = pat match {
    case Constr(name, pats) =>
      arg match {
        case Obj(`name`, args) =>
          matches(pats, args, env)
        case _ => fail
      }

    case Wildcard =>
      env

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case _ =>
      fail
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail

    case Case(pats, body) :: rest =>
      {
        val env = matches(pats, args, lex)
        eval(body, env, dyn)
      } or {
        apply(rest, args, lex, dyn)
      }
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = {
    val oldindent = indent
    indent += "  "
    val res = _eval(expr, lex, dyn)
    indent = oldindent
    // println(indent + expr + " ~> " + res + " in " + lex)
    res
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map { eval(_, lex, dyn) }
  }

  def _eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case Eq(lhs, rhs) =>
      if (equal(eval(lhs, lex, dyn), eval(rhs, lex, dyn)))
        True
      else
        False

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

    case Lambda(cases) =>
      Closure(cases, lex)
  }
}