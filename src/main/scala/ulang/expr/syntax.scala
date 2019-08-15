package ulang.expr

import ulang.Pretty

sealed trait Pat extends Pretty

sealed trait Expr extends Pretty {
  def toPat: Pat = this match {
    case id: Id => id
    case App(fun, arg) => UnApp(fun.toPat, arg.toPat)
    case _ => ulang.error("not a pattern: " + this)
  }
}

sealed trait Val extends Pretty
sealed trait Norm extends Val
sealed trait Data extends Norm
sealed trait Const extends Data

case class Lit(any: Any) extends Expr with Const

sealed trait Id extends Expr with Pat {
  def name: String
}

object Id extends (String => Id) {
  def isTag(name: String) = {
    name.head.isUpper || operators.data.exists(_.name == name)
  }

  def apply(name: String) = {
    if (isTag(name))
      Tag(name)
    else
      Var(name)
  }

  def unapply(id: Id) = {
    Some(id.name)
  }
}
case class Tag(name: String) extends Id with Const

case class Var(name: String) extends Id {
  def in(pat: Pat): Boolean = pat match {
    case Wildcard | _: Lit | _: Tag =>
      false
    case that: Var =>
      this == that
    case UnApp(fun, arg) =>
      (this in fun) || (this in arg)
    case Named(pat, x) =>
      (this in pat) || (this == x)
    case Cond(pat, cond) =>
      (this in pat) // || (this in cond)
  }

  def in(expr: Expr): Boolean = expr match {
    case _: Lit | _: Tag =>
      false
    case that: Var =>
      this == that
    case App(fun, arg) =>
      (this in fun) || (this in arg)
    case Lambda(cases) =>
      cases exists (this in _)
  }

  def in(cs: Case): Boolean = {
    val Case(pats, body) = cs
    (this in body) && !(pats exists (this in _))
  }
}

case object Wildcard extends Pat
case class Named(pat: Pat, name: Var) extends Pat
case class Cond(pat: Pat, cond: Expr) extends Pat
case class UnApp(fun: Pat, arg: Pat) extends Pat

case class App(fun: Expr, arg: Expr) extends Expr

case class Case(pats: List[Pat], body: Expr) extends Pretty {
  def arity = pats.length
}

case class Lambda(cases: List[Case]) extends Expr {
  assert(!cases.isEmpty)
  assert(cases forall (_.arity == arity))
  def arity = cases.head.arity
}

object Lambda extends ((List[Pat], Expr) => Expr) {
  def apply(pats: List[Pat], body: Expr) = {
    Lambda(List(Case(pats, body)))
  }
}

case class Defer(expr: Expr, lex: Env, dyn: Env) extends Val {
  lazy val norm = eval.norm(expr, lex, dyn)
}

case class Obj(fun: Data, arg: Val) extends Data

case class Fun(cases: List[Case], rargs: List[Val], lex: Env) extends Norm {
  assert(!cases.isEmpty)
  assert(cases forall (_.arity == arity))
  assert(rargs.length <= arity)
  def arity = cases.head.arity
  def isComplete = arity == rargs.length
}
