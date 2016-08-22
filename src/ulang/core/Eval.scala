package ulang.core

import arse._
import ulang._

object Eval {
  import Pat._

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, arg, lex, dyn) or { sys.error("cannot apply " + fun + " to " + arg) }
      
    case Prim(apply) =>
      apply(arg) or { sys.error("cannot apply " + fun + " to " + arg) }

    case data: Data =>
      Obj(data, arg)

    case _ =>
      sys.error("not a function " + fun)
  }

  def apply(cases: List[Case], arg: Val, lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail

    case Case(pat, body) :: rest =>
      {
        val env = bind(pat, arg, Env.empty)
        eval(body, lex ++ env, dyn)
      } or {
        apply(rest, arg, lex, dyn)
      }
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map { eval(_, lex, dyn) }
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case id @ Id(name) if isTag(name) =>
      id

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case Id(name) =>
      val bound = lex.keys ++ dyn.keys
      sys.error("unbound identifier " + name + " in " + bound.mkString("[", " ", "]"))

    case LetIn(pat, arg, body) =>
      eval(body, bind(pat, eval(arg, lex, dyn), lex), dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case res => sys.error("not a boolean value: " + res)
      }

    case Apply(fun, arg) =>
      apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)

    case Match(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Bind(cases) =>
      Closure(cases, lex)
  }
}