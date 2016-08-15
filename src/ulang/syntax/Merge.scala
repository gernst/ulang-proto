package ulang.syntax

import ulang._

object Merge {
  def merge(expr: Expr): Expr = expr match {
    case Apply(fun, arg) =>
      Apply(merge(fun), merge(arg))
    case Match(cases) =>
      Match(merge(cases))
    case _ =>
      expr
  }

  def merge(cases: List[Case]): List[Case] = {
    val patterns = cases.map(_.pattern).distinct
    val grouped = cases.groupBy(_.pattern)
    patterns.map {
      pat =>
        Case(pat, merges(grouped(pat).map(_.body)))
    }
  }

  def merges(exprs: List[Expr]): Expr = {
    assert(!exprs.isEmpty)

    if (exprs.length == 1) {
      exprs.head
    } else {
      val cases = exprs.collect {
        case Match(cases) =>
          cases
        case _ =>
          sys.error("overlapping matches " + exprs)
      }.flatten

      Match(merge(cases))
    }
  }
}