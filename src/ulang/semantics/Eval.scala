package ulang.semantics

import arse._

import ulang._

object Eval {
  var indent = "eval   "

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, arg, lex, dyn)

    case _ =>
      ??? // 
  }

  def matches(pat: Expr, arg: Val, env: Env): Env = {
    val res = _matches(pat, arg, env)
    // println(indent + pat + " | " + arg + " ~> " + res + " <~ " + env)
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
        case Obj(`name`, args) =>
          matches(pats, args, env)
        case _ => fail
      }
      
    case syntax.Wildcard =>
      env

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
      ???

    case syntax.Case(pat, body) :: rest =>
      {
        // val env = matches(pat, arg, Env.empty)
        // eval(body, lex ++ env, dyn)
        val env = matches(pat, arg, lex) // can handle non-linear patterns this way, however, shadowing is impossible
        eval(body, env, dyn)
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
      
    case syntax.Eq(arg1, arg2) =>
      if(equal(eval(arg1, lex, dyn), eval(arg2, lex, dyn)))
        True
      else
        False

    case syntax.LetIn(pat, arg, body) =>
      eval(body, matches(pat, eval(arg, lex, dyn), lex), dyn)

    case syntax.IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case _ => ???
      }

    case syntax.Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case syntax.Match(cases) =>
      Closure(cases, lex)
  }
}