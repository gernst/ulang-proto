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
  def bind(pat: Pat, arg: Val, env: Env): Env = pat match {
    case Wildcard =>
      env

    case id: Id =>
      env + (id -> arg)

    case tag: Tag =>
      if (tag == norm(arg)) env
      else backtrack()

    case SubPat(id, pat) =>
      bind(pat, arg, env + (id -> arg))

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
    case Bind(pat, body, lex) =>
      val env = bind(pat, arg, lex)
      // val newlex = env ++ lex
      /* cond.map(eval(_, newlex)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => ulang.error("not a boolean in pattern condition: " + res)
      } */
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
      case const: Const => List(const)
      case Fun(_, res) => res
    }

    (binds.flatten, consts.flatten) match {
      case (Nil, Nil) => sys.error("undefined")
      case (Nil, List(res)) => res
      case (Nil, _) => sys.error("non-deterministic")
      case (binds, res) => Fun(binds, res)
    }
  }

  def apply(fun: Norm, arg: Val, dyn: Env): Norm = fun match {
    case tag: Tag => Obj(tag, arg)
    case Fun(cases, res) => merge(apply(cases, arg, dyn))
    case _ => sys.error("not a function: " + fun)
  }

  def defer(cases: List[Case], lex: Env): List[Bind] = {
    cases map { case Case(pat, body) => Bind(pat, body, lex) }
  }

  def defer(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case tag: Tag => tag
    case id: Id if lex contains id => lex(id)
    case id: Id if dyn contains id => dyn(id)
    case Lambda(cases) => Fun(defer(cases, lex))
    case _ => Defer(expr, lex, dyn)
  }

  def norm(arg: Val): Norm = arg match {
    case d: Defer => d.norm
    case n: Norm => n
  }

  def eval(expr: Expr, dyn: Env): Norm = {
    val lex = Env.empty
    eval(expr, lex, dyn)
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Norm = expr match {
    case tag: Tag => tag
    case id: Id if lex contains id => norm(lex(id))
    case id: Id if dyn contains id => norm(dyn(id))
    case Lambda(cases) => Fun(defer(cases, lex))
    case App(fun, arg) => apply(eval(fun, lex, dyn), defer(arg, lex, dyn), dyn)
  }
}