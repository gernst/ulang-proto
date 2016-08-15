package ulang.calculus.linear

import Math.abs

import ulang._
import ulang.syntax._

object Number {
  def apply(n: Int): Expr = {
    if (n == 0) Zero
    else Succ(apply(n - 1))
  }

  def unapply(e: Expr): Option[Int] = e match {
    case Zero    => Some(0)
    case Succ(e) => unapply(e).map(_ + 1)
    case _       => None
  }
}

// a polynom where summands is a list of
// coefficient/expression terms (ai, xi)
// represents the term
//   a1 * x1 + ... + an + c

case class Polynomial(summands: List[(Int, Expr)], const: Int) {
  lazy val (coefficients, terms) = summands.unzip

  def toExpr = {
    val pairs = summands collect {
      case (1, x)  => x
      case (-1, x) => UnaryMinus(x)
      case (a, x)  => Mult(Number(a), x)
    }
    if (pairs.isEmpty)
      Number(const)
    else if (const == 0)
      pairs.reduceRight(Plus(_, _))
    else
      pairs.foldRight(Number(const): Expr)(Plus(_, _))
  }

  def ::(ax: (Int, Expr)) = Polynomial(ax :: summands, const)

  def *(i: Int) = Polynomial(summands map { case (a, x) => (a * i, x) }, const * i)
  def /(i: Int) = Polynomial(summands map { case (a, x) => (a / i, x) }, const / i)
  def mod(i: Int) = Polynomial(summands map { case (a, x) => (mod_(a, i), x) }, mod_(const, i))

  def +(i: Int) = Polynomial(summands, const + i)

  def -(i: Int) = this + (-i)
  def unary_-() = this * (-1)

  def +(that: Polynomial) = {
    Polynomial(this.summands ++ that.summands, this.const + that.const)
  }

  def -(that: Polynomial) = {
    this + (-that)
  }

  def *(that: Polynomial) = {
    val Polynomial(axs, i) = this
    val Polynomial(bys, j) = that

    var czs: List[(Int, Expr)] = Nil

    for ((a, x) <- axs; (b, y) <- bys) {
      czs = (a * b, Mult(x, y)) :: czs
    }

    for ((a, x) <- axs) {
      czs = (a * j, x) :: czs
    }

    for ((b, y) <- bys) {
      czs = (b * i, y) :: czs
    }

    Polynomial(czs, i * j)
  }

  def reduce = {
    val pairs = summands.groupBy(_._2).map {
      case (x, as) =>
        (x, as.map(_._1).sum)
    }.toList

    val nontrivial = pairs.collect {
      case (x, a) if a != 0 => (a, x)
    }

    val newsummands = nontrivial // .sortBy(_._2)(ExprOrdering)

    Polynomial(newsummands, const)
  }

  def normalize = {
    val g = gcd(const :: coefficients)
    if (g != 0) this / g else this
  }

  def unit = {
    coefficients indexWhere (abs(_) == 1)
  }

  def isConstant = {
    summands.isEmpty
  }

  def solve(k: Int) = {
    val (s1, (b, y) :: s2) = summands.splitAt(k)

    val newcoefficients = (s1 ++ s2) map {
      case (a, x) =>
        assert(a % b == 0) // works for +1/-1 in particular
        (-a / b, x)
    }
    assert(const % b == 0)
    val newconst = const / b

    (y, Polynomial(newcoefficients, newconst))
  }
}

object Polynomial {
  def apply(expr: Expr): Polynomial = expr match {
    /* case Numint(n, _) =>
      Polynomial(Nil, n.toInt)

    case PlusOne(n) =>
      apply(n) + 1

    case MinusOne(n) =>
      apply(n) - 1

    case UnaryMinus(n) =>
      -apply(n)

    case Plus(m, n) =>
      apply(m) + apply(n)

    case Minus(m, n) =>
      apply(Plus(m, UnaryMinus(n)))

    case Mult(m, n) =>
      apply(m) * apply(n)
      */

    case _ =>
      Polynomial(List((1, expr)), 0)
  }
}