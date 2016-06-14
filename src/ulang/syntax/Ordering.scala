package ulang.syntax

import ulang._

object CaseOrdering extends Ordering[Case] {
  def compare(a: Case, b: Case): Int = (a, b) match {
    case (Case(pattern1, body1), Case(pattern2, body2)) =>
      val p = ExprOrdering.compare(pattern1, pattern2)
      if (p < 0) -1
      else if (p == 0) ExprOrdering.compare(body1, body2)
      else 1
  }
}

object ExprOrdering extends Ordering[Expr] {
  def compare(a: Expr, b: Expr): Int = (a, b) match {
    case _ if a == b =>
      0

    case (Id(name1), Id(name2)) =>
      name1 compare name2

    case (_: Id, _) =>
      -1

    case (Apply(fun1, arg1), Apply(fun2, arg2)) =>
      ulang.compare((fun1, arg1), (fun2, arg2))

    case (_: Apply, _) =>
      -1

    case (Match(cases1), Match(cases2)) =>
      val scases1 = cases1.sorted(CaseOrdering)
      val scases2 = cases2.sorted(CaseOrdering)
      ulang.compare(scases1, scases2)

    case _ =>
      1
  }
}
