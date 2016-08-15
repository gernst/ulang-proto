package ulang.semantics

import arse._

import ulang._

case class Closure(cases: List[syntax.Case], lex: Env) extends Pretty

object Eval {
  var indent = "eval   "

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, arg, lex, dyn)

    case fun: Data =>
      semantics.Apply(fun, arg)

    case fun: (Val => Val) @unchecked =>
      fun(arg)

    case _ =>
      Undefined
  }

  def matches(pattern: Expr, arg: Val, env: Env): Env = {
    val res = _matches(pattern, arg, env)
    // println(indent + pattern + " | " + arg + " ~> " + res)
    res
  }

  def _matches(pattern: Expr, arg: Val, env: Env): Env = pattern match {
    case Constr(name) =>
      arg match {
        case Constr(`name`) => env
        case _              => fail
      }

    case Var(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None        => env + (name -> arg)
        case _           => fail
      }

    case syntax.Apply(fun1, arg1) =>
      arg match {
        case semantics.Apply(fun2, arg2) =>
          matches(arg1, arg2, matches(fun1, fun2, env))
        case _ =>
          fail
      }

    case _ =>
      fail
  }

  def apply(cases: List[syntax.Case], arg: Val, lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      Undefined

    case syntax.Case(pattern, body) :: rest =>
      {
        val env = matches(pattern, arg, Env.empty)
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

  def _eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case Constr(name) =>
      expr

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case syntax.LetIn(Id(name), arg, body) =>
      eval(body, lex + (name -> eval(arg, lex, dyn)), dyn)

    case syntax.IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True  => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case _     => Undefined
      }

    case syntax.Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case syntax.Match(cases) =>
      Closure(cases, lex)
  }
}