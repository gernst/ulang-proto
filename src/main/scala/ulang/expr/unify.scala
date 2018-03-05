package ulang.expr

import arse.control._

object unify {
  def test(pat1: List[Pat], pat2: List[Pat]) = {
    val u = new unify;
    { u.unify(pat1, pat2); true } or { false }
  }

  def apply(pat1: List[Pat], pat2: List[Pat]) = {
    val u = new unify;
    {
      u.unify(pat1, pat2)
      Some(u.rep): Option[Subst]
    } or {
      None
    }
  }
}

class unify {
  var rep = Subst.empty

  def find(a: Pat): Pat = {
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

  def union(p1: Id, p2: Pat) {
    rep += (find(p1) -> find(p2))
  }

  def unify(ps1: List[Pat], ps2: List[Pat]): Unit = {
    if (ps1.length != ps2.length)
      backtrack()

    for ((p1, p2) <- (ps1, ps2).zipped)
      unify(p1, p2)
  }

  def unify(p1: Pat, p2: Pat): Unit = (find(p1), find(p2)) match {
    case (r1, r2) if r1 == r2 =>

    case (id: Id, a) =>
      union(id, a)

    case (a, id: Id) =>
      unify(id, a)

    case (UnApp(fun1, args1), UnApp(fun2, args2)) =>
      unify(fun1, fun2)
      unify(args1, args2)

    case _ =>
      backtrack()
  }
}