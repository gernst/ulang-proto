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
    case Named(pat, _) =>
      this in pat
    case Cond(pat, cond) =>
      (this in pat) || (this in cond)
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
    val Case(pat, body) = cs
    (this in body) && !(this in pat)
  }
}

case object Wildcard extends Pat
case class Named(pat: Pat, name: Id) extends Pat
case class Cond(pat: Pat, cond: Expr) extends Pat
case class UnApp(fun: Pat, arg: Pat) extends Pat

case class App(fun: Expr, arg: Expr) extends Expr

case class Case(pat: Pat, body: Expr) extends Pretty

case class Lambda(cases: List[Case]) extends Expr

object Lambda extends ((Pat, Expr) => Expr) {
  def apply(pat: Pat, body: Expr) = {
    Lambda(List(Case(pat, body)))
  }
}

case class Defer(expr: Expr, lex: Env, dyn: Env) extends Val {
  lazy val norm = eval.eval(expr, lex, dyn)
}

case class Obj(fun: Data, arg: Val) extends Data
case class Bind(pat: Pat, body: Expr, lex: Env) extends Pretty
case class Fun(binds: List[Bind], res: List[Data] = Nil) extends Norm
