package ulang.expr

import bk.Control
import bk.backtrack
import ulang.Pretty
import ulang.shell

trait Eq

case class Clos(cases: List[Case], lex: Env) extends Pretty
case class Obj(tag: Tag, args: List[Val]) extends Pretty with Eq

object Env {
  val empty: Env = Map.empty
  val default: Env = Map("=" -> builtin.equal, "print" -> builtin.print)
}

object eval {
  def bind(pat: Pat, arg: Val, dyn: Env, env: Env): Env = pat match {
    case Wildcard =>
      env

    case Lit(any) =>
      if (any == arg) env
      else backtrack()

    case id @ Tag(_) =>
      if (id == arg) env
      else backtrack()

    case Id(name) =>
      (env get name) match {
        case Some(that) if builtin.equal.test(that, arg) => env
        case None => env + (name -> arg)
        case _ => backtrack()
      }

    case SubPat(name, pat) =>
      bind(pat, arg, dyn, env + (name -> arg))

    case UnApp(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, dyn, bind(pfun, vfun, dyn, env))
        case _ =>
          backtrack()
      }
  }

  def bind(pats: List[Pat], args: List[Val], dyn: Env, env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, dyn, bind(pat, arg, dyn, env))

    case _ =>
      backtrack()
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Env): Val = cs match {
    case Case(pats, cond, body) =>
      val env = bind(pats, args, dyn, Env.empty)
      val newlex = lex ++ env
      cond.map(eval(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => shell.error("not a boolean in pattern: " + res)
      }
      eval(body, newlex, dyn)
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case tag: Tag =>
      Obj(tag, args)

    case Clos(cases, lex) =>
      apply(cases, args, lex, dyn) or shell.error(fun + " mismatches " + args.mkString(" "))

    case f: (List[Val] => Val) @unchecked =>
      f(args)

    case _ =>
      shell.error("not a function " + fun)
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map (eval(_, lex, dyn))
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case tag: Tag =>
      tag

    case Lit(any) =>
      any

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case Id(name) =>
      val bound = lex.keys ++ dyn.keys
      shell.error("unbound identifier " + name + " in " + bound.mkString("[", " ", "]"))

    case LetIn(eqs, body) =>
      val bindings = eqs.map {
        case LetEq(pat, arg) => (pat, eval(arg, lex, dyn))
      }
      val (pats, args) = bindings.unzip
      val env = bind(pats, args, dyn, Env.empty)
      val newlex = lex ++ env
      eval(body, newlex, dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case builtin.True => eval(arg1, lex, dyn)
        case builtin.False => eval(arg2, lex, dyn)
        case res => shell.error("not a boolean in test: " + res)
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