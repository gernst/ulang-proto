package ulang.semantics

import ulang._

object Eval {
  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case _ =>
      sys.error("not a function: " + fun)
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case syntax.Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case syntax.Lambda(Id(name), body) =>
      (arg: Val, dyn: Env) =>
        eval(body, lex + (name -> arg), dyn)
  }
}