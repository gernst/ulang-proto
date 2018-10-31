package ulang.expr

import bk._
import scala.annotation.tailrec

case class Net(ops: Map[Tag, Net], app: Option[Net], bound: Option[Net], rhs: List[Expr]) {
  // missing: forcing of expressions
  def bind(exprs: List[Expr], env: Stack): (List[Expr], Stack) = exprs match {
    case Nil =>
      (rhs, env)

    case (op: Tag) :: rest if ops contains op =>
      ops(op) bind (rest, env)

    case App(fun, arg) :: rest if !app.isEmpty =>
      app.get bind (fun :: arg :: rest, env)

    case expr :: rest if !bound.isEmpty =>
      bound.get bind (rest, expr :: env)

    case _ =>
      backtrack()
  }

  def insert(pats: List[Pat], env: Stack, expr: Expr): Net = pats match {
    case Nil =>
      copy(rhs = rhs :+ expr)

    case (op: Tag) :: rest =>
      val sub = ops.getOrElse(op, Net.empty)
      copy(ops = ops + (op -> sub.insert(rest, env, expr)))

    case UnApp(fun, arg) :: rest =>
      val sub = app.getOrElse(Net.empty)
      copy(app = Some(sub.insert(fun :: arg :: rest, env, expr)))

    case (fv: Free) :: rest =>
      val sub = bound.getOrElse(Net.empty)
      copy(bound = Some(sub.insert(rest, fv :: env, expr)))
  }
}

object Net {
  val empty = Net(Map.empty, None, None, Nil)
}