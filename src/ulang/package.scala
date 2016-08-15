package object ulang {
  type Expr = syntax.Expr

  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  def compare[A, B](x: (A, B), y: (A, B))(implicit ev1: Ordering[A], ev2: Ordering[B]): Int = {
    val (a1, b1) = x
    val (a2, b2) = y
    val c = ev1.compare(a1, a2)
    if (c != 0) c
    else ev2.compare(b1, b2)
  }

  def compare[A](xs: List[A], ys: List[A])(implicit ev: Ordering[A]): Int = (xs, ys) match {
    case (Nil, Nil) =>
      0
    case (Nil, _) =>
      -1
    case (_, Nil) =>
      1
    case (x :: xs, y :: ys) =>
      val c = ev.compare(x, y)
      if (c != 0) c
      else compare(xs, ys)
  }
}