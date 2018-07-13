package ulang.expr

import bk.Control
import bk.backtrack
import ulang.Pretty

trait Eq

case class Prim(name: String, arity: Int, fun: List[Val] => Val, curried: List[Val] = Nil) extends Val with (Val => Val) {
  assert(arity >= 0)

  def apply(arg: Val) = {
    if (arity == 0) fun(curried.reverse)
    else Prim(name, arity - 1, fun, arg :: curried)
  }
}

case class Clos(cases: List[Case], lex: Stack) extends Val
case class Obj(tag: Data, arg: Val) extends Data with Eq

object Objs extends ((Data, List[Val]) => Val) {
  def apply(fun: Data, args: List[Val]): Val = {
    args.foldLeft(fun)(Obj)
  }

  def flatten(any: Data, args: List[Val]): (Data, List[Val]) = any match {
    case Obj(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (any, args)
  }

  def unapply(any: Data): Option[(Data, List[Val])] = {
    Some(flatten(any, Nil))
  }
}

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
      if (builtin.test(arg, env(index))) env
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

  def apply(cs: Case, arg: Val, lex: Stack, dyn: Env): Val = cs match {
    case Case(pats, body) =>
      val env = bind(pats, arg, lex)
      // val newlex = env ++ lex
      /* cond.map(eval(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => ulang.error("not a boolean in pattern condition: " + res)
      } */
      eval(body, env, dyn)
  }

  def apply(cases: List[Case], arg: Val, lex: Stack, dyn: Env): Val = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, arg, lex, dyn) or apply(rest, arg, lex, dyn)
  }

  def apply(fun: Val, arg: Val, dyn: Env): Val = fun match {
    case data: Data =>
      Obj(data, arg)

    case Clos(cases, lex) =>
      apply(cases, arg, lex, dyn) or ulang.error(fun + " mismatches " + arg)

    case f: (List[Val] => Val) @unchecked =>
      f(List(arg))

    case _ =>
      ulang.error("not a function " + fun)
  }

  def eval(expr: Expr, dyn: Env): Val = {
    eval(expr, Stack.empty, dyn)
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

    case App(fun, arg) =>
      val res = apply(eval(fun, lex, dyn), eval(arg, lex, dyn), dyn)
      res

    case MatchWith(arg, cases) =>
      apply(cases, eval(arg, lex, dyn), lex, dyn)

    case Lambda(cases) =>
      Clos(cases, lex)
  }
}