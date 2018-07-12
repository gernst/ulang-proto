package ulang.expr

import bk.Control
import bk.backtrack
import ulang.Pretty

trait Eq

abstract class Prim(name: String) extends Val with (List[Val] => Val)
case class Clos(cases: List[Case], lex: Stack) extends Val
case class Obj(tag: Tag, args: List[Val]) extends Val with Eq

object Env {
  val empty: Env = Map.empty
  val default: Env = Map("=" -> builtin.equal, "print" -> builtin.print)

  def apply(dfs: List[(Free, Expr)], lex: Stack): Env = {
    dfs.foldLeft(default) {
      case (dyn, (Free(name), rhs)) =>
        dyn + (name -> eval.eval(rhs, lex, dyn))
    }
  }
}

object Stack {
  val empty: Stack = List.empty
}

object eval {
  def bind(pat: Pat, arg: Val, env: Stack): Stack = pat match {
    case Wildcard =>
      env

    case lit: Lit =>
      if (lit == arg) env
      else backtrack()

    case id: Tag =>
      if (id == arg) env
      else backtrack()

    case Bound(index) =>
      assert(index < env.length)
      if (builtin.equal.test(arg, env(index))) env
      else backtrack()

    case Free(name) =>
      arg :: env

    case SubPat(bound, pat) =>
      bind(pat, arg, arg :: env)

    case UnApp(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, bind(pfun, vfun, env))
        case _ =>
          backtrack()
      }
  }

  def bind(pats: List[Pat], args: List[Val], env: Stack): Stack = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, bind(pat, arg, env)) // right to left evaluation

    case _ =>
      backtrack()
  }

  def apply(cs: Case, args: List[Val], lex: Stack, dyn: Env): Val = cs match {
    case Case(pats, cond, body) =>
      val env = bind(pats, args, Stack.empty)
      val newlex = env ++ lex
      cond.map(eval(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => ulang.error("not a boolean in pattern: " + res)
      }
      eval(body, newlex, dyn)
  }

  def apply(cases: List[Case], args: List[Val], lex: Stack, dyn: Env): Val = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case tag: Tag =>
      Obj(tag, args)

    case Clos(cases, lex) =>
      apply(cases, args, lex, dyn) or ulang.error(fun + " mismatches " + args.mkString(" "))

    case f: (List[Val] => Val) @unchecked =>
      f(args)

    case _ =>
      ulang.error("not a function " + fun)
  }

  def eval(expr: Expr, dyn: Env): Val = {
    eval(expr, Stack.empty, dyn)
  }

  def eval(exprs: List[Expr], lex: Stack, dyn: Env): List[Val] = {
    exprs map (eval(_, lex, dyn))
  }

  def eval(expr: Expr, lex: Stack, dyn: Env): Val = expr match {
    case tag: Tag =>
      tag

    case lit: Lit =>
      lit

    case Bound(index) =>
      assert(index < lex.length)
      lex(index)

    case Free(name) =>
      if (dyn contains name)
        dyn(name)
      else
        ulang.error("unbound identifier " + name + " in " + dyn.keys.mkString("[", " ", "]"))

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case builtin.True => eval(arg1, lex, dyn)
        case builtin.False => eval(arg2, lex, dyn)
        case res => ulang.error("not a boolean in test: " + res)
      }

    case App(fun, args) =>
      val res = apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)
      res

    case MatchWith(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Lambda(cases) =>
      Clos(cases, lex)
  }
}