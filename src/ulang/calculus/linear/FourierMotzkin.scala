package ulang.calculus.linear

import scala.math._

object FourierMotzkin {
  /*def fm(cs: List[Constraint], j: Int): List[Constraint] = {
    assert(cs forall (_.isLeq))

    val rows = cs map (c => Vector(c.as: _*))
    val A = Matrix(rows: _*)
    val b = Vector(cs map (_.c): _*)
    val (_D, d) = fm(A, b, j)

    ???
  }*/

  // Compute for a problem A * x <= b
  // an equi-solvable problem D * x <= d 
  // using Fourier-Motzkin-Elimination of the variable x_j,
  // resulting in D(j) = 0. 
  // Based on: https://de.wikipedia.org/wiki/Fourier-Motzkin-Elimination

  def fm(A: Matrix, b: Vector, j: Int): (Matrix, Vector) = {
    assert(j < A.cols)

    // indices ranging over the inequalities of the problem
    val M: Seq[Int] = 0 until A.rows

    // kth unit vector for entries in U corresponding to Z
    def e(k: Int) = Vector.unit(A.rows, k)

    // difference entry in U for a pair in N x P
    def u(np: (Int, Int)) = np match {
      case (n, p) =>
        (e(n) * A(p, j)) - (e(p) * A(n, j))
    }

    // partition lines according to the sign of the coefficient of x_j
    val Z = M filter (A(_, j) == 0)
    val N = M filter (A(_, j) < 0)
    val P = M filter (A(_, j) > 0)

    // cross product containing all negative/positive pairs
    val NxP = for (n <- N; p <- P) yield (n, p)

    // first part of U: columns containing unit vectors
    val UZ = Z map e
    
    // second part of U: columns containing difference entries
    val UNxP = NxP map u

    val U = Matrix(UZ ++ UNxP: _*)

    val D = U * A
    val d = U * b

    (D, d)
  }

  def fm(A: Matrix, b: Vector): Int = {
    val N: Seq[Int] = (0 until A.cols)
    val (_, d) = N.foldLeft((A, b)) {
      case ((_D, d), j) => fm(_D, d, j)
    }
    assert(d.length == 1)
    d(0)
  }
}