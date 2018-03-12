package ulang.expr

import ulang.Pretty

sealed trait Pat extends Pretty

sealed trait Expr extends Pretty

case object Wildcard extends Pat

case class Lit(any: Any) extends Expr with Pat with Eq

sealed trait Atom extends Expr with Pat { def name: String }
case class Tag(name: String) extends Atom with Eq
case class Id(name: String) extends Atom

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