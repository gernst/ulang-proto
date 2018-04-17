package ulang.expr

import ulang.Pretty

sealed trait Pat extends Pretty

sealed trait Expr extends Pretty {
  def toPat: Pat = this match {
    case atom: Atom => atom
    case App(fun, args) => UnApp(fun.toPat, args map (_.toPat))
    case _ => ulang.error("not a pattern: " + this)
  }
}

case class Lit(any: Any) extends Expr with Pat with Eq

sealed trait Atom extends Expr with Pat { def name: String }
case class Tag(name: String) extends Atom with Eq

case class Id(name: String) extends Atom {
  def in(pat: Pat): Boolean = pat match {
    case Wildcard | _: Lit | _: Tag =>
      false
    case that: Id =>
      this == that
    case UnApp(fun, args) =>
      (this in fun) || (args exists (this in _))
    case SubPat(_, pat) =>
      this in pat
  }

  def in(expr: Expr): Boolean = expr match {
    case _: Lit | _: Tag =>
      false
    case that: Id =>
      this == that
    case App(fun, args) =>
      (this in fun) || (args exists (this in _))
    case Lambda(cases) =>
      cases exists (this in _)
    case MatchWith(args, cases) =>
      (args exists (this in _)) || (cases exists (this in _))
    case LetIn(eqs, body) =>
      (eqs exists (this in _.arg)) || (this in body) && !(eqs exists (this in _.pat))
    case IfThenElse(test, iftrue, iffalse) =>
      (this in test) || (this in iftrue) || (this in iffalse)
  }

  def in(cs: Case): Boolean = {
    val Case(pats, cond, body) = cs
    val free = (this in body) || (cond exists (this in _))
    val bound = (pats exists (this in _))
    free && !bound
  }
}

case object Wildcard extends Pat
case class SubPat(name: String, pat: Pat) extends Pat
case class UnApp(fun: Pat, args: List[Pat]) extends Pat // { assert(!args.isEmpty) }
case class App(fun: Expr, args: List[Expr]) extends Expr // { assert(!args.isEmpty) }

case class Case(pats: List[Pat], cond: Option[Expr], body: Expr) extends Pretty
case class Lambda(cases: List[Case]) extends Expr

case class MatchWith(args: List[Expr], cases: List[Case]) extends Expr

case class LetEq(pat: Pat, arg: Expr) extends Pretty
case class LetIn(eqs: List[LetEq], body: Expr) extends Expr

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr

object Atom extends (String => Atom) {
  def isTag(name: String) = {
    name.head.isUpper || operators.data.exists(_.name == name)
  }

  def apply(name: String) = {
    if (isTag(name))
      Tag(name)
    else
      Id(name)
  }

  def unapply(atom: Atom) = {
    Some(atom.name)
  }
}