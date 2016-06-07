package ulang.semantics

import ulang._

object Eval {
  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(Id(name), body, lex) =>
      eval(body, lex + (name -> arg), dyn)

    case fun: Data =>
      semantics.Apply(fun, arg)

    case fun: (Val => Val) @unchecked =>
      fun(arg)

    case _ =>
      sys.error("not a function: " + fun)
  }
  
  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case syntax.Constr(name) =>
      expr

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case syntax.Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case syntax.Lambda(bound, body) =>
      Closure(bound, body, lex)
  }
}