package ulang.prove

import ulang.expr.Expr
import ulang.expr.Var
import ulang.expr.App
import ulang.expr.Apps

class congruence {
  val emptyUse: Set[Expr] = Set()

  var rep = Map[Expr, Expr]()
  var use = Map[Expr, Set[Expr]]() withDefaultValue emptyUse
  var sig = Map[Expr, Expr]()

  def find(a: Expr): Expr = {
    val b = rep.getOrElse(a, a)
    if (a == b) {
      a
    } else {
      val c = find(b)
      // path compression
      rep += (a -> c)
      c
    }
  }

  def union(e1: Expr, e2: Expr) {
    rep += (find(e1) -> find(e2))
  }

  def canon(e: Expr): Expr = e match {
    case App(fun, args) =>
      canonsig(App(canon(fun), canon(args)))
    case _ =>
      canonsig(e)
  }

  def ++=(es: List[(Expr, Expr)]) = {
    for ((e1, e2) <- es)
      this += (e1, e2)
  }

  def +=(e1: Expr, e2: Expr) = {
    merge(canon(e1), canon(e2))
  }

  def canon(es: List[Expr]): List[Expr] = {
    es map canon
  }

  def replace(e: Expr, e1: Expr, e2: Expr): Expr = e match {
    case `e1` => e2
    case App(fun, arg) => App(replace(fun, e1, e2), replace(arg, e1, e2))
    case _ => e
  }

  def merge(e1: Expr, e2: Expr) {
    if (e1 != e2) {
      union(e1, e2)
      for (u <- use(e1)) {
        sig += u -> replace(sig(u), e1, e2)
        for (v <- use(e2) if sig(v) == sig(u)) {
          merge(find(u), find(v))
        }
        use += e2 -> (use(e2) + u)
      }
    }
  }

  def canonsig(e: Expr): Expr = e match {
    case Apps(fun, args) =>
      args flatMap use find (sig(_) == e) match {
        case Some(u) =>
          find(u)
        case None =>
          for (arg <- args)
            use += arg -> (use(arg) + e)
          sig += e -> e
          use -= e
          e
      }
    case _ =>
      find(e)
  }
}