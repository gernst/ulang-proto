package ulang.syntax

import ulang._

// Note: use of this makes the theory trivial
//       (can discern semantically equivalent terms)

object Reify {
  def apply(name: String, args: Expr*): Expr = {
    args.foldLeft(Id(name): Expr)(Apply)
  }

  def list(exprs: List[Expr]): Expr = {
    exprs.foldRight(apply("Nil"))(apply("Cons", _, _))
  }

  def reify(expr: Expr): Expr = expr match {
    case name: Id =>
      apply("Id", name)

    case Apply(fun, arg) =>
      apply("Apply", reify(fun), reify(arg))

    case Match(cases) =>
      val args = cases map {
        case Case(bound, body) =>
          apply("Case", reify(bound), reify(body))
      }
      apply("Match", list(args))
  }
}