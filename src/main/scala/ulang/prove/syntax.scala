package ulang.prove

import ulang.expr.Expr
import ulang.expr.Pat

sealed trait Rule

case object Sorry extends Rule

case object Trivial extends Rule
case class Cut(expr: Expr) extends Rule

case class Case(pat: Pat, rule: Rule)
case class Induction(expr: Expr, cases: List[Case]) extends Rule