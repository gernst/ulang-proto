package ulang

import arse._
import ulang._

trait Val

case class Clos(cases: List[Case], lex: Env) extends Val

case class Obj(id: Id, args: List[Val]) extends Val {
  override def toString = this match {
    case Obj(Op(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Obj(Op(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Obj(Op(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case _ =>
      (id :: args).mkString("(", " ", ")")
  }
}

object interpreter {
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
  

  def bind(pats: List[Expr], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, bind(pat, arg, env))

    case (_, Nil) =>
      sys.error("missing arguments for " + pats.mkString(" "))

    case (Nil, _) =>
      sys.error("extra arguments " + args.mkString(" "))
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Env): Val = cs match {
    case Case(pats, body) =>
      val env = bind(pats, args, Env.empty)
      eval(body, lex ++ env, dyn)
  }
  

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      sys.error("no case for " + args.mkString(" "))

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  
  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case id @ Tag(_) =>
      Obj(id, args)

    case Clos(cases, lex) =>
      apply(cases, args, lex, dyn) or sys.error("cannot apply " + fun + " to " + args.mkString(" "))

    case _ =>
      sys.error("not a function " + fun)
  }
  

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map (eval(_, lex, dyn))
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

    case LetIn(pat, _arg, body) =>
      val arg = eval(_arg, lex, dyn)
      val env = bind(pat, arg, lex) or sys.error("cannot bind " + pat + " to " + arg)
      eval(body, env, dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case res => sys.error("not a boolean value: " + res)
      }

    case Apply(fun, args) =>
      apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)

    case Match(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Bind(cases) =>
      Clos(cases, lex)
  }

  def eval(mod: Module, lex: Env, _dyn: Env): Env = mod match {
    case Module(defs) =>
      var dyn = _dyn

      val funs = defs.collect {
        case Def(Apply(Id(name), args), rhs) if !args.isEmpty =>
          (name, Case(args, rhs))
      }

      val consts = defs.collect {
        case Def(Id(name), rhs) =>
          (name, rhs)
      }

      for ((name, cases) <- _root_.ulang.group(funs)) {
        val rhs = Bind(cases)
        val fun = eval(rhs, lex, dyn)
        dyn += (name -> fun)
      }

      for ((name, rhs) <- consts) {
        val const = eval(rhs, lex, dyn)
        dyn += (name -> const)
      }

      dyn
  }
}