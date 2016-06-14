package ulang.calculus

import ulang._
import ulang.syntax._

class Congruence(var _cong: DisjointSets[Expr], var _use: Map[Expr, Set[Expr]], var _sig: Map[Expr, Expr]) {
  def find(e: Expr) = _cong find e
  def union(e1: Expr, e2: Expr) { _cong = _cong union (e1, e2) }
  def use(e: Expr) = _use.getOrElse(e, Set.empty)
  def sig(e: Expr) = _sig(e)

  def merge(e1: Expr, e2: Expr) = {
    val cc = new Congruence(_cong, _use, _sig)
    cc _process (e1, e2)
    cc
  }

  def canon(e: Expr): Expr = e match {
    case Apply(fun, arg) =>
      canonsig(Apply(canon(fun), canon(arg)))
    case _ =>
      canonsig(e)
  }

  private def _process(e1: Expr, e2: Expr) {
    _merge(canon(e1), canon(e2))
  }

  private def replace(e: Expr, a: Expr, b: Expr): Expr = e match {
    case Applys(op, args) =>
      Applys(op, args map { c => if (a == c) b else c })

    case _ =>
      e
  }

  private def _merge(e1: Expr, e2: Expr) {
    if (e1 != e2) {
      union(e1, e2)
      for (u <- use(e1)) {
        _sig += u -> (replace(sig(u), e1, e2))
        for (v <- use(e2) if sig(v) == sig(u)) {
          _merge(find(u), find(v))
        }
        _use += e2 -> (use(e2) + u)
      }
    }
  }

  private def canonsig(e: Expr): Expr = e match {
    case Apply(fun, arg) =>
      use(arg) find (sig(_) == e) match {
        case Some(u) =>
          find(u)
        case None =>
          _use += arg -> (use(arg) + e)
          _sig += e -> e
          _use -= e
          e
      }
    case _ =>
      find(e)
  }

  override def toString = _cong.toString + " | use: " + _use + " | sig: " + _sig
}

object Congruence {
  def empty = new Congruence(DisjointSets.empty, Map.empty, Map.empty)
}