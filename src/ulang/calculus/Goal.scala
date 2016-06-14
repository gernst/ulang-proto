package ulang.calculus

import ulang._
import ulang.syntax._

sealed trait Goal {
  def ::(phi: Expr): Goal
  def canon(expr: Expr): Expr
  def merge(lhs: Expr, rhs: Expr): Goal
  def contains(phi: Expr): Boolean
  def reverse: Goal
}

case object Closed extends Goal {
  def ::(phi: Expr) = this
  def canon(expr: Expr) = expr
  def merge(lhs: Expr, rhs: Expr) = this
  def contains(phi: Expr) = false
  def reverse = this
}

case class Open(cong: Congruence, phis: List[Expr]) extends Goal {
  def ::(phi: Expr) = Open(cong, phi :: phis)
  def canon(expr: Expr) = cong canon expr
  def merge(lhs: Expr, rhs: Expr) = Open(cong merge (lhs, rhs), phis)
  def contains(phi: Expr) = phis contains phi
  def reverse = Open(cong, phis.reverse)
  override def toString = cong + " | " + phis.mkString(", ")
}

object Goal {
  val empty = Open(Congruence.empty, Nil)
}
