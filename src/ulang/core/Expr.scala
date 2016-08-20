package ulang.core

sealed trait Pat
sealed trait Expr

case class Id(name: String) extends Expr with Pat {
  assert(!name.isEmpty)
}

case object Wildcard extends Pat
case class Constr(tag: String, args: List[Pat]) extends Pat

case class Apply(fun: Expr, args: List[Expr]) extends Expr

case class Case(pats: List[Pat], body: Expr)
case class Lambda(cases: List[Case]) extends Expr
case class Match(args: List[Expr], cases: List[Case]) extends Expr

case class Eq(lhs: Expr, rhs: Expr) extends Expr
case class LetIn(pat: Pat, arg: Expr, body: Expr) extends Expr
case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr
