package ulang.command

import ulang._
import ulang.syntax._
import ulang.calculus._

object Lemmas extends Handler {
  val rule = Simplify

  def prove(phi: Expr): Proof = {
    rule(Seq(List(Not(phi))))
  }

  def apply(exprs: List[Expr]): List[Proof] = {
    exprs map prove
  }
}