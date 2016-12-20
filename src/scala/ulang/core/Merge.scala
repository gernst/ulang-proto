package ulang.core

import ulang._

object Merge extends (List[Expr] => Expr) {
  def merge(expr: Expr): Expr = expr match {
    case Apply(fun, arg) =>
      Apply(merge(fun), merge(arg))
    case Bind(cases) =>
      Bind(merge(cases))
    case _ =>
      expr
  }

  def merge(cases: List[Case]): List[Case] = {
    val patterns = cases.map(_.pat).distinct
    val grouped = cases.groupBy(_.pat)
    patterns.map {
      pat =>
        Case(pat, apply(grouped(pat).map(_.body)))
    }
  }

  def apply(exprs: List[Expr]): Expr = {
    assert(!exprs.isEmpty)

    if (exprs.length == 1) {
      exprs.head
    } else {
      val cases = exprs.flatMap {
        case Bind(cases) =>
          cases
        case _ =>
          sys.error("overlapping matches " + exprs)
      }

      Bind(merge(cases))
    }
  }
}