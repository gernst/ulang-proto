package ulang.calculus.linear

case class Vector(length: Int) {
  val data = Array.ofDim[Int](length)

  def apply(i: Int) = data(i)
  def update(i: Int, v: Int) { data(i) = v }

  def *(that: Vector): Int = {
    assert(this.length == that.length)

    var res: Int = 0
    for (i <- 0 until length)
      res += this(i) * that(i)

    res
  }

  def *(that: Matrix): Vector = {
    assert(this.length == that.rows)

    val res = Vector(that.cols)
    for (
      i <- 0 until that.rows;
      j <- 0 until that.cols
    ) res(j) += this(i) * that(i, j)

    res
  }

  def +(that: Vector): Vector = {
    assert(this.length == that.length)

    val res = Vector(length)
    for (i <- 0 until length)
      res(i) = this(i) + that(i)

    res
  }

  def -(that: Vector): Vector = {
    assert(this.length == that.length)

    val res = Vector(length)
    for (i <- 0 until length)
      res(i) = this(i) - that(i)

    res
  }

  def *(k: Int): Vector = {
    val res = Vector(length)
    for (i <- 0 until length)
      res(i) = k * this(i)
    res
  }

  override def toString = {
    data.mkString("[", ", ", "]")
  }
}

object Vector {
  def apply(vs: Int*): Vector = {
    val res = Vector(vs.length)
    for (i <- 0 until vs.length)
      res(i) = vs(i)
    res
  }

  def unit(length: Int, k: Int) = {
    val res = Vector(length)
    res(k) = 1
    res
  }
}