package ulang.calculus.linear

import Math.abs

import ulang._
import ulang.syntax._

object Omega {
  // part of the solver of the omega test
  def reduce(cs: Constraint, forbidden: List[Id]): (Id, Int, Constraint) = {
    assert(cs.isEq)

    val p = cs.p
    assert(p.unit < 0)

    val (ak, k) = p.coefficients.zipWithIndex minBy { case (a, k) => abs(a) }
    val xk = p.terms(k)
    val m = abs(ak) + 1

    val y: Id = ???

    val q = p mod m

    // coefficient at k is now unit
    assert(abs(q.coefficients(k)) == 1)

    // shift k because the fresh y has been prepended
    (y, k + 1, Constraint((m, y) :: q.reduce, cs.op))
  }
}