package ulang.calculus.linear

case class Matrix(rows: Int, cols: Int) {
  val data = Array.ofDim[Int](rows, cols)

  def apply(i: Int, j: Int) = data(i)(j)
  def update(i: Int, j: Int, v: Int) { data(i)(j) = v }

  def transpose: Matrix = {
    val res = Matrix(cols, rows)
    for (
      i <- 0 until rows;
      j <- 0 until cols
    ) res(j, i) = this(i, j)
    res
  }

  def *(that: Matrix): Matrix = {
    assert(this.cols == that.rows)

    val res = Matrix(this.rows, that.cols)
    for (
      i <- 0 until this.rows;
      j <- 0 until that.cols;
      r <- 0 until this.cols
    ) res(i, j) += this(i, r) * that(r, j)

    res
  }

  def *(that: Vector): Vector = {
    assert(this.cols == that.length)

    val res = Vector(this.rows)
    for (
      i <- 0 until this.rows;
      j <- 0 until this.cols
    ) res(i) += this(i, j) * that(j)

    res
  }

  def +(that: Matrix): Matrix = {
    assert(this.cols == that.cols)
    assert(this.rows == that.rows)

    val res = Matrix(rows, rows)
    for (
      i <- 0 until rows;
      j <- 0 until cols
    ) res(i, j) = this(i, j) + that(i, j)

    res
  }

  override def toString = {
    val lines = data.map(_.mkString("[", ", ", "]"))
    lines.mkString("[", "\n ", "]")
  }
}

object Matrix {
  def apply(vs: Vector*): Matrix = {
    assert(!vs.isEmpty)
    val rows = vs.length
    val cols = vs(0).length
    assert(vs.forall(_.length == cols))

    val res = Matrix(rows, cols)
    for (
      i <- 0 until rows;
      j <- 0 until cols
    ) res(i, j) = vs(i)(j)

    res
  }
}