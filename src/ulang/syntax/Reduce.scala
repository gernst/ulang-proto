package ulang.syntax

import arse._
import ulang._

object Reduce {
  var indent = "reduce "

  def apply(fun: Expr, arg: Expr, dyn: Subst): Expr = fun match {
    case syntax.Match(cases) =>
      apply(cases, arg, dyn)
    case _ =>
      syntax.Apply(fun, arg)
  }

  def apply(cases: List[syntax.Case], arg: Expr, dyn: Subst): Expr = cases match {
    case Nil =>
      ???

    case syntax.Case(pattern, body) :: rest =>
      {
        val env = matches(pattern, arg, Subst.empty)
        reduce(body, env, dyn)
      } or {
        apply(rest, arg, dyn)
      }
  }

  def matches(pattern: Expr, arg: Expr, env: Subst): Subst = {
    val res = _matches(pattern, arg, env)
    println(indent + pattern + " | " + arg + " ~> " + res)
    res
  }

  def _matches(pattern: Expr, arg: Expr, env: Subst): Subst = pattern match {
    case Constr(name, args) =>
      ???
      /*
      arg match {
        case Constr(`name`) => env
        case _              => fail
      }
      */

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env // TODO: dubious semantics
        case None        => env + (name -> arg)
        case _           => fail
      }

    case syntax.Apply(fun1, arg1) =>
      arg match {
        case syntax.Apply(fun2, arg2) =>
          matches(arg1, arg2, matches(fun1, fun2, env))
        case _ =>
          fail
      }

    case _ =>
      fail
  }

  def reduce(cs: syntax.Case, lex: Subst, dyn: Subst): syntax.Case = cs match {
    // TODO: beware of the bindings
    case syntax.Case(bound, body) =>
       val fv = syntax.free(bound)
       syntax.Case(reduce(bound, Subst.empty, dyn -- fv), reduce(body, lex -- fv, dyn -- fv))
  }

  def reduce(expr: Expr, lex: Subst, dyn: Subst): Expr = {
    val oldindent = indent
    indent += "  "
    val res = _reduce(expr, lex, dyn)
    indent = oldindent
    println(indent + expr + " ~> " + res + " in " + lex)
    res
  }

  def _reduce(expr: Expr, lex: Subst, dyn: Subst): Expr = expr match {
    case Constr(name, args) =>
      ???
      /// expr

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case id: Id =>
      id

    case syntax.IfThenElse(test, arg1, arg2) =>
      val rtest = reduce(test, lex, dyn)
      val rarg1 = reduce(arg1, lex, dyn)
      val rarg2 = reduce(arg2, lex, dyn)

      rtest match {
        case True  => rarg1
        case False => rarg2
        case _     => syntax.IfThenElse(rtest, rarg1, rarg2)
      }

    case syntax.Apply(fun, arg) =>
      val rfun = reduce(fun, lex, dyn)
      val rarg = reduce(arg, lex, dyn)
      apply(rfun, rarg, dyn)

    case syntax.Match(cases) =>
      syntax.Match(cases map (reduce(_, lex, dyn)))
  }
}