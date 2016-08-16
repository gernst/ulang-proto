package ulang.syntax

import arse._
import ulang._

object Reduce {
  var indent = "reduce "

  def apply(fun: Expr, arg: Expr, dyn: Subst): Expr = fun match {
    case Match(cases) =>
      {
        apply(cases, arg, dyn)
      } or {
        Apply(fun, arg)
      }

    case _ =>
      Apply(fun, arg)
  }

  def apply(cases: List[Case], arg: Expr, dyn: Subst): Expr = cases match {
    case Nil =>
      fail

    case Case(pat, body) :: rest =>
      {
        val env = matches(pat, arg, Subst.empty)
        reduce(body, env, dyn)
      } or {
        apply(rest, arg, dyn)
      }
  }

  def matches(pat: Expr, arg: Expr, env: Subst): Subst = {
    val res = _matches(pat, arg, env)
    println(indent + pat + " | " + arg + " ~> " + res)
    res
  }

  def matches(pats: List[Expr], args: List[Expr], env: Subst): Subst = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      matches(pats, args, matches(pat, arg, env))

    case _ =>
      fail
  }

  def _matches(pat: Expr, arg: Expr, env: Subst): Subst = pat match {
    case Constr(name, pats) =>
      arg match {
        case Constr(`name`, args) =>
          matches(pats, args, env)
        case _ => fail
      }

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env // TODO: dubious semantics
        case None => env + (name -> arg)
        case _ => fail
      }

    case Apply(fun1, arg1) =>
      arg match {
        case Apply(fun2, arg2) =>
          matches(arg1, arg2, matches(fun1, fun2, env))
        case _ =>
          fail
      }

    case _ =>
      fail
  }

  def reduce(cs: Case, lex: Subst, dyn: Subst): Case = cs match {
    // TODO: beware of the bindings
    case Case(bound, body) =>
      val fv = free(bound)
      Case(reduce(bound, Subst.empty, dyn -- fv), reduce(body, lex -- fv, dyn -- fv))
  }

  def reduce(expr: Expr, lex: Subst, dyn: Subst): Expr = {
    val oldindent = indent
    indent += "  "
    val res = _reduce(expr, lex, dyn)
    indent = oldindent
    println(indent + expr + " ~> " + res + " in " + lex)
    res
  }

  def reduce(exprs: List[Expr], lex: Subst, dyn: Subst): List[Expr] = {
    exprs map { reduce(_, lex, dyn) }
  }

  def _reduce(expr: Expr, lex: Subst, dyn: Subst): Expr = expr match {
    case Constr(name, args) =>
      Constr(name, reduce(args, lex, dyn))

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case id: Id =>
      id

    case IfThenElse(test, arg1, arg2) =>
      val rtest = reduce(test, lex, dyn)
      val rarg1 = reduce(arg1, lex, dyn)
      val rarg2 = reduce(arg2, lex, dyn)

      rtest match {
        case True => rarg1
        case False => rarg2
        case _ => IfThenElse(rtest, rarg1, rarg2)
      }

    case Apply(fun, arg) =>
      val rfun = reduce(fun, lex, dyn)
      val rarg = reduce(arg, lex, dyn)
      apply(rfun, rarg, dyn)

    case Match(cases) =>
      Match(cases map (reduce(_, lex, dyn)))
  }
}