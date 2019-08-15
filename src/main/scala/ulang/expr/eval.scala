package ulang.expr

import bk.Control
import bk.backtrack
import ulang.Pretty

/* case class Prim(name: String, arity: Int, fun: List[Expr] => Expr, curried: List[Expr] = Nil) extends Expr with (Expr => Expr) {
  assert(arity >= 0)

  def apply(arg: Expr) = {
    if (arity == 0) fun(curried.reverse)
    else Prim(name, arity - 1, fun, arg :: curried)
  }
} */

object Env {
  val empty: Env = Map.empty
  val default: Env = empty // Map("=" -> builtin.equal, "print" -> builtin.print)

  def apply(dfs: List[(Var, Expr)]): Env = {
    dfs.foldLeft(default) {
      case (dyn, (x, rhs)) =>
        dyn + (x -> eval.eval(rhs, dyn))
    }
  }
}

object eval {
  def equal(v1: Val, v2: Val): Boolean = {
    return const(v1) == const(v2)
  }

  def bind(pat: Pat, arg: Val, env: Env): Env = pat match {
    case Wildcard =>
      env

    case x: Var if env contains x =>
      if (equal(env(x), arg)) env
      else backtrack()

    case x: Var =>
      env + (x -> arg)

    case tag: Tag =>
      if (tag == force(arg)) env
      else backtrack()

    case Named(pat, x) =>
      bind(pat, arg, bind(x, arg, env))

    case Cond(pat, cond) =>
      val newenv = bind(pat, arg, env)
      if (equal(arg, builtin.True)) newenv
      else backtrack()

    case UnApp(fun1, arg1) =>
      force(arg) match {
        case Obj(fun2, arg2) =>
          bind(arg1, arg2, bind(fun1, fun2, env))
        case _ =>
          backtrack()
      }

    case _ =>
      backtrack()
  }

  def bind(pats: List[Pat], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env
    case (pat :: pats, arg :: args) =>
      bind(pats, args, bind(pat, arg, env))
    case _ =>
      sys.error("argument length mismatch: " + pats.mkString(" ") + " and " + args.mkString(" "))
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Env): Norm = cs match {
    case Case(pats, body) =>
      norm(body, bind(pats, args, lex), dyn)
  }

  def apply(fun: Fun, cases: List[Case], args: List[Val], lex: Env, dyn: Env): Norm = cases match {
    case Nil =>
      sys.error("cannot apply " + fun + " to " + args.mkString(" "))

    case cs :: rest =>
      { apply(cs, args, lex, dyn) } or { apply(fun, rest, args, lex, dyn) }
  }

  def apply(fun: Fun, dyn: Env): Norm = {
    if (fun.isComplete) {
      val cases = fun.cases
      val args = fun.rargs.reverse
      val lex = fun.lex
      apply(fun, cases, args, lex, dyn)
    } else {
      fun
    }
  }

  def push(fun: Norm, arg: Val, dyn: Env): Norm = fun match {
    case data: Data =>
      Obj(data, arg)
    case Fun(cases, rargs, lex) =>
      apply(Fun(cases, arg :: rargs, lex), dyn)
    case _ =>
      sys.error("not a function: " + fun)
  }

  def force(arg: Val): Norm = arg match {
    case defer: Defer => defer.norm
    case norm: Norm => norm
  }

  def defer(expr: Expr, lex: Env, dyn: Env): Val = {
    Defer(expr, lex, dyn)
  }

  def const(arg: Val): Data = arg match {
    case defer: Defer => const(defer.norm)
    case tag: Tag => tag
    case Obj(fun, arg) => Obj(const(fun), const(arg))
    case _ => sys.error("not constant: " + arg)
  }

  def norm(expr: Expr, lex: Env, dyn: Env): Norm = expr match {
    case x: Var if lex contains x => force(lex(x))
    case x: Var if dyn contains x => force(dyn(x))
    case x: Var => sys.error("unbound variable: " + x + " in " + lex + " and " + dyn)
    case tag: Tag => tag
    case Lambda(cases) => Fun(cases, Nil, lex)
    case App(fun, arg) => push(norm(fun, lex, dyn), defer(arg, lex, dyn), dyn)
  }

  def eval(expr: Expr, dyn: Env) = {
    norm(expr, Env.empty, dyn)
  }

  def strict(expr: Expr, dyn: Env) = {
    const(eval(expr, dyn))
  }
}