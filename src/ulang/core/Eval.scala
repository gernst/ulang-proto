package ulang.core

import arse._
import ulang._

object Eval {
  def bind(pat: Expr, arg: Val, env: Env): Env = pat match {
    case Wildcard =>
      env

    case id @ Tag(_) =>
      if (id == arg) env
      else fail

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case Apply(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, bind(pfun, vfun, env))
        case _ =>
          fail
      }

    case _ =>
      fail
  }

  def apply(cs: Case, arg: Val, lex: Env, dyn: Env): Val = cs match {
    case Case(pat, body) =>
      val env = bind(pat, arg, Env.empty)
      eval(body, lex ++ env, dyn)
  }

  def apply(cases: List[Case], arg: Val, lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail

    case cs :: rest =>
      apply(cs, arg, lex, dyn) or apply(rest, arg, lex, dyn)
  }

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case Closure(cases, lex) =>
      apply(cases, arg, lex, dyn) or sys.error("cannot apply " + fun + " to " + arg)

    case data: Data =>
      Obj(data, arg)

    case fun: (Val => Val) @unchecked =>
      fun(arg) or sys.error("cannot apply " + fun + " to " + arg)

    case _ =>
      sys.error("not a function " + fun)
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case id @ Tag(_) =>
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