package ulang

import arse.control._
import ulang._
import java.io.File

trait Eq

case class Clos(cases: List[Case], lex: Env) extends Pretty
case class Prim(name: String, f: List[Val] => Val) extends Pretty
case class Obj(tag: Tag, args: List[Val]) extends Pretty with Eq

case class Lazy(body: Expr, lex: Env) extends Pretty {
  var memo: Option[Val] = None
  def force(dyn: Env) = {
    if (memo == None)
      memo = Some(interpreter.eval(body, lex, dyn))
    memo.get
  }
}

object Env {
  val empty: Env = Map.empty
  val default: Env = Map("=" -> builtin.equal, "print" -> builtin.print)
}

object interpreter {
  def bind(pat: Pat, arg: Val, dyn: Env, env: Env): Env = pat match {
    case Wildcard =>
      env

    case Lit(any) =>
      if (any == arg) env
      else fail

    case id @ Tag(_) =>
      if (id == arg) env
      else fail

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case SubPat(name, pat) =>
      bind(pat, arg, dyn, env + (name -> arg))

    case Force(pat) =>
      arg match {
        case arg @ Lazy(body, lex) =>
          val inner = arg.force(dyn)
          bind(pat, inner, dyn, env)
        case _ =>
          fail
      }

    case UnApp(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, dyn, bind(pfun, vfun, dyn, env))
        case _ =>
          fail
      }
  }

  def bind(pats: List[Pat], args: List[Val], dyn: Env, env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, dyn, bind(pat, arg, dyn, env))

    case _ =>
      fail
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Env): Val = cs match {
    case Case(pats, cond, body) =>
      val env = bind(pats, args, dyn, Env.empty)
      val newlex = lex ++ env
      cond.map(eval(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => fail
        case res => sys.error("not a condition in pattern: " + res)
      }
      eval(body, newlex, dyn)
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case tag: Tag =>
      Obj(tag, args)

    case Clos(cases, lex) =>
      apply(cases, args, lex, dyn) or sys.error(fun + " mismatches " + args.mkString(" "))

    case Prim(_, f) =>
      f(args)

    case _ =>
      sys.error("not a function " + fun)
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
      sys.error("unbound identifier " + name + " in " + bound.mkString("[", " ", "]"))

    case LetIn(pat, _arg, body) =>
      val arg = eval(_arg, lex, dyn)
      val env = bind(pat, arg, dyn, lex) or sys.error("cannot bind " + pat + " to " + arg)
      eval(body, env, dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case builtin.True => eval(arg1, lex, dyn)
        case builtin.False => eval(arg2, lex, dyn)
        case res => sys.error("not a condition in test: " + res)
      }

    case Susp(body) =>
      Lazy(body, lex)

    case App(fun, args) =>     
      val res = apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)
      res

    case MatchWith(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Bind(cases) =>
      Clos(cases, lex)
  }

  def eval(df: Def, lex: Env, dyn: Env): (String, Val) = df match {
    case Def(Id(name), None, rhs) =>
      (name -> eval(rhs, lex, dyn))
  }
}