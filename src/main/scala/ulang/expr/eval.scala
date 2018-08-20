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
  var current: Env = default

  def apply(dfs: List[(Free, Expr)], lex: Stack): Env = {
    dfs.foldLeft(default) {
      case (dyn, (Free(name), rhs)) =>
        dyn + (name -> eval.eval(rhs, lex))
    }
  }
}

object Stack {
  val empty: Stack = List.empty
}

object eval {
  def hasEq(e: Expr) = e match {
    case _: HasEq => true
    case _ => false
  }

  def isEq(e1: Expr, e2: Expr): Boolean = {
    val f1 = e1.force
    val f2 = e2.force

    if (!hasEq(f1) || !hasEq(f2))
      sys.error("equality not supported for " + f1 + " and " + f2)

    (e1, e2) match {
      case (Lit(any1), Lit(any2)) =>
        any1 == any2
      case (Tag(name1), Tag(name2)) =>
        name1 == name2
      case (App(fun1, arg1), App(fun2, arg2)) =>
        return isEq(fun1, fun2) && isEq(arg1, arg2)
      case _ =>
        false
    }
  }

  def bind(pat: Pat, arg: Expr, env: Stack): Stack = pat match {
    case Wildcard =>
      env

    case lit: Lit =>
      if (lit == arg) env
      else backtrack()

    case id: Tag =>
      if (id == arg.force) env
      else backtrack()

    case Bound(index) =>
      assert(index < env.length)
      if (isEq(arg, env(index))) env
      else backtrack()

    case Free(name) =>
      arg :: env

    case SubPat(bound, pat) =>
      bind(pat, arg, arg :: env)

    case UnApp(fun1, arg1) =>
      arg.force match {
        case App(fun2, arg2) =>
          bind(arg1, arg2, bind(fun1, fun2, env))
        case _ =>
          backtrack()
      }
  }

  def apply(cs: Case, arg: Expr, lex: Stack): Expr = cs match {
    case Case(pats, body) =>
      val env = bind(pats, arg, lex)
      // val newlex = env ++ lex
      /* cond.map(eval(_, newlex)).foreach {
        case builtin.True =>
        case builtin.False => backtrack()
        case res => ulang.error("not a boolean in pattern condition: " + res)
      } */
      eval(body, env)
  }

  def apply(cases: List[Case], arg: Expr, lex: Stack): Expr = cases match {
    case Nil =>
      backtrack()

    case cs :: rest =>
      apply(cs, arg, lex) or apply(rest, arg, lex)
  }

  def apply(fun: Expr, arg: Expr): Expr = fun match {
    case Lazy(Lambda(cases), env) =>
      apply(cases, arg, env) or ulang.error(fun + " mismatches " + arg)

    case Lit(f: (List[Expr] => Expr) @unchecked) =>
      f(List(arg))

    case _ =>
      App(fun, arg)

    case _ =>
      ulang.error("not a function " + fun)
  }

  def defer(expr: Expr, lex: Stack): Expr = expr match {
    case _: Tag =>
      expr
    case Bound(index) =>
      lex(index)
    case _: Lazy =>
      sys.error("unexpected lazy value " + expr + " in defer")
    case _ =>
      Lazy(expr, lex)
  }

  def eval(expr: Expr): Expr = {
    eval(expr, Stack.empty)
  }

  def eval(expr: Expr, lex: Stack): Expr = expr match {
    case tag: Tag =>
      tag

    case lit: Lit =>
      lit

    case _: Lazy =>
      expr

    case _: Lambda =>
      defer(expr, lex)

    case Bound(index) =>
      assert(index < lex.length)
      lex(index)

    case Free(name) =>
      val dyn = Env.current
      if (dyn contains name) eval(dyn(name), lex)
      else ulang.error("unbound identifier " + name + " in " + dyn.keys.mkString("[", " ", "]"))

    case App(fun, arg) =>
      val res = apply(eval(fun, lex), defer(arg, lex))
      res

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex) match {
        case builtin.True => eval(arg1, lex)
        case builtin.False => eval(arg2, lex)
        case res => ulang.error("not a boolean in test: " + res)
      }

    case MatchWith(arg, cases) =>
      apply(cases, eval(arg, lex), lex)
  }
}