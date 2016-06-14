package ulang.calculus

import ulang._
import ulang.syntax._

case class Step(prems: List[Proof], concl: Seq, rule: Rule) extends Proof {
  def isClosed = prems forall (_.isClosed)
}

case class Seq(phis: List[Expr]) extends Proof {
  def ant = phis filter {
    case Not(_) => false
    case _      => true
  }

  def suc = phis collect {
    case Not(phi) => phi
  }

  def ::(phi: Expr) = Seq(phi :: phis)
  def isClosed = false
}

object Seq {
  def apply(ant: List[Expr], suc: List[Expr]): Seq = {
    Seq(ant ::: (suc map Not))
  }
}