package ulang

import arse._

object Subst {
  val empty: Subst = Map()
}

class unify {
  var rep = Subst.empty

  def find(a: Expr): Expr = {
    val b = rep.getOrElse(a, a)
    if (a == b) {
      a
    } else {
      val c = find(b)
      rep += (a -> c)
      c
    }
  }

  def union(e1: Id, e2: Expr) {
    rep += (find(e1) -> find(e2))
  }

  def unify(es1: List[Expr], es2: List[Expr]): Unit = {
    if (es1.length != es2.length)
      fail

    for ((e1, e2) <- (es1, es2).zipped)
      unify(e1, e2)
  }

  def unify(e1: Expr, e2: Expr): Unit = (find(e1), find(e2)) match {
    case (r1, r2) if r1 == r2 =>

    case (id: Id, a) =>
      union(id, a)

    case (a, id: Id) =>
      unify(id, a)

    case (App(fun1, args1), App(fun2, args2)) =>
      unify(fun1, fun2)
      unify(args1, args2)

    case _ =>
      fail
  }

  // Option({ unify(e1, e2); rep } or { null })
}