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

  def apply(dfs: List[(Var, Expr)], dyn: Env): Env = {
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
      ???

    case x: Var =>
      env + (x -> arg)

    case tag: Tag =>
      if (tag == norm(arg)) env
      else backtrack()

    case SubPat(x, pat) =>
      bind(pat, arg, env + (x -> arg))

    case UnApp(fun1, arg1) =>
      norm(arg) match {
        case Obj(fun2, arg2) =>
          bind(arg1, arg2, bind(fun1, fun2, env))
        case _ =>
          backtrack()
      }

    case _ =>
      backtrack()
  }

  def apply(bn: Bind, arg: Val, dyn: Env): Norm = bn match {
    case Bind(pat, cond, body, lex) =>
      val env = bind(pat, arg, lex)

      cond.map(eval(_, env, dyn)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => ulang.error("not a boolean in pattern condition: " + res)
      }

      eval(body, env, dyn)
  }

  def apply(cases: List[Bind], arg: Val, dyn: Env): List[Norm] = cases match {
    case Nil =>
      Nil

    case bind :: rest =>
      { apply(bind, arg, dyn) :: apply(rest, arg, dyn) } or { apply(rest, arg, dyn) }
  }

  def merge(norms: List[Norm]): Norm = {
    val binds = norms collect {
      case Fun(binds, _) => binds
    }

    val consts = norms collect {
      case Fun(_, res) => res
      case res: Data => List(res)
      case res => ulang.error("dropped result: " + res)
    }

    (binds.flatten, consts.flatten) match {
      case (Nil, Nil) =>
        ulang.error("undefined")
      case (Nil, List(res)) => res
      case (Nil, res) => ulang.error("non-deterministic result: " + res.mkString("[", ", ", "]"))
      case (binds, res) => Fun(binds, res)
    }
  }

  def apply(fun: Norm, arg: Val, dyn: Env): Norm = fun match {
    case obj: Data => Obj(obj, arg)
    case Fun(cases, res) => merge(apply(cases, arg, dyn))
    case _ => ulang.error("not a function: " + fun)
  }

  def defer(cases: List[Case], lex: Env): List[Bind] = {
    cases map { case Case(pat, cond, body) => Bind(pat, cond, body, lex) }
  }

  def defer(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case tag: Tag => tag
    /* case x: Var if lex contains x => lex(x)
    case x: Var if dyn contains x => dyn(x)
    case x: Var => ulang.error("unbound variable: " + x) */
    case Lambda(cases) => Fun(defer(cases, lex))
    case _ => Defer(expr, lex, dyn)
  }

  def norm(arg: Val): Norm = arg match {
    case d: Defer => d.norm
    case n: Norm => n
  }

  def const(arg: Val): Data = arg match {
    case c: Const => c
    case d: Defer => const(d.norm)
    case Obj(fun, arg) => Obj(const(fun), const(arg))
    case Fun(Nil, List(res)) => res
    case _: Fun => ulang.error("not constant: " + arg)
  }

  def eval(expr: Expr, dyn: Env): Norm = {
    val lex = Env.empty
    eval(expr, lex, dyn)
  }

  def strict(expr: Expr, dyn: Env): Data = {
    const(eval(expr, dyn))
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Norm = expr match {
    case tag: Tag => tag
    case x: Var if lex contains x => norm(lex(x))
    case x: Var if dyn contains x => norm(dyn(x))
    case x: Var => ulang.error("unbound variable: " + x)
    case Lambda(cases) => Fun(defer(cases, lex))
    case App(fun, arg) => apply(eval(fun, lex, dyn), defer(arg, lex, dyn), dyn)
  }
}