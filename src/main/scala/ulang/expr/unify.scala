package ulang.expr

import bk.Control
import bk.backtrack

object unify {
  def test(pat1: List[Pat], pat2: List[Pat]) = {
    val u = new unify;
    { u.unify(pat1, pat2); true } or { false }
  }

  def apply(pat1: Pat, pat2: Pat): Option[Map[Pat, Pat]] = {
    apply(List(pat1), List(pat2))
  }

  def apply(pats1: List[Pat], pats2: List[Pat]): Option[Map[Pat, Pat]] = {
    val u = new unify;
    {
      u.unify(pats1, pats2)
      Some(u.rep): Option[Map[Pat, Pat]]
    } or {
      None
    }
  }

  def unbind(p: Pat): Pat = p match {
    case _: Id | Wildcard => Wildcard
    case _: Tag | _: Lit => p
    case SubPat(name, pat) => unbind(pat)
    case UnApp(fun, args) => UnApp(unbind(fun), args.map(unbind))
  }

  def merges(ps1: List[Pat], ps2: List[Pat]): List[Pat] = {
    (ps1, ps2).zipped map {
      case (p1, p2) => merge(p1, p2)
    }
  }

  def merge(p1: Pat, p2: Pat): Pat = (p1, p2) match {
    case (SubPat(name1, pat1), SubPat(name2, pat2)) =>
      merge(pat1, pat2)
    case (UnApp(fun1, args1), UnApp(fun2, args2)) =>
      UnApp(merge(fun1, fun2), merges(args1, args2))
    case _ =>
      if (p1 == p2) p1 else Wildcard
  }
}

class unify {
  var rep = Map[Pat, Pat]()

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