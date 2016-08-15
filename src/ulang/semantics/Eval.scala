package ulang.semantics

import arse._

import ulang._

object Eval {
  var indent = "eval   "

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, arg, lex, dyn)

    case Undefined =>
      Undefined

    case _ =>
      ??? // 
  }

  def matches(pat: Expr, arg: Val, env: Env): Env = {
    val res = _matches(pat, arg, env)
    // println(indent + pat + " | " + arg + " ~> " + res)
    res
  }

  def matches(pats: List[Expr], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      matches(pats, args, matches(pat, arg, env))

    case _ =>
      fail
  }

  def _matches(pat: Expr, arg: Val, env: Env): Env = pat match {
    case syntax.Constr(name, pats) =>
      arg match {
        case Obj(`name`, args: List[_]) =>
          matches(pats, args, env)
        case _ => fail
      }

    case syntax.Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case _ =>
      fail
  }

  def apply(cases: List[syntax.Case], arg: Val, lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      Undefined

    case syntax.Case(pat, body) :: rest =>
      {
        val env = matches(pat, arg, Env.empty)
        eval(body, lex ++ env, dyn)
      } or {
        apply(rest, arg, lex, dyn)
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
    case syntax.Constr(name, args) =>
      Obj(name, eval(args, lex, dyn))

    case syntax.Id(name) if lex contains name =>
      lex(name)

    case syntax.Id(name) if dyn contains name =>
      dyn(name)

    case syntax.LetIn(syntax.Id(name), arg, body) =>
      eval(body, lex + (name -> eval(arg, lex, dyn)), dyn)

    case syntax.IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case _ => Undefined
      }

    case syntax.Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case syntax.Match(cases) =>
      Closure(cases, lex)
  }
}