package ulang.syntax

import ulang._

case class Apply(fun: Expr, arg: Expr) extends Expr

case class Case(pattern: Expr, body: Expr) extends Ordered[Case] {
  def compare(that: Case) = CaseOrdering.compare(this, that)
}

case class Match(cases: List[Case]) extends Expr

object Lambda extends ((Expr, Expr) => Expr) {
  def apply(bound: Expr, body: Expr): Expr = {
    Match(List(Case(bound, body)))
  }

  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
    case Match(List(Case(bound, body))) =>
      Some((bound, body))
    case _ =>
      None
  }
}

object Bind extends ((Id, Expr, Expr) => Expr) {
  def apply(op: Id, bound: Expr, body: Expr): Expr = {
    Apply(op, Lambda(bound, body))
  }

  def unapply(expr: Expr): Option[(Id, Expr, Expr)] = expr match {
    case Apply(op: Id, Lambda(bound, body)) =>
      Some((op, bound, body))
    case _ =>
      None
  }
}

object Applys extends ((Expr, List[Expr]) => Expr) {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(Apply)
  }

  def unapply(expr: Expr): Option[(Expr, List[Expr])] = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Expr, args: List[Expr]): (Expr, List[Expr]) = expr match {
    case Apply(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }
}

object Lambdas extends ((List[Expr], Expr) => Expr) {
  def apply(bounds: List[Expr], body: Expr): Expr = {
    bounds.foldRight(body)(Lambda)
  }

  def unapply(expr: Expr): Option[(List[Expr], Expr)] = {
    Some(flatten(expr))
  }

  def flatten(expr: Expr): (List[Expr], Expr) = expr match {
    case Lambda(bound, body) =>
      val (bounds, inner) = flatten(body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}

object Binds extends ((Id, List[Id], Expr) => Expr) {
  def apply(op: Id, bounds: List[Id], body: Expr): Expr = {
    bounds.foldRight(body)(Bind(op, _, _))
  }

  def unapply(expr: Expr): Option[(Id, List[Expr], Expr)] = expr match {
    case Bind(op, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      Some((op, bound :: bounds, inner))
    case _ =>
      None
  }

  def flatten(op: Id, expr: Expr): (List[Expr], Expr) = expr match {
    case Bind(`op`, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}

class Unary(name: String) extends (Expr => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(`op`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Expr) = {
    Apply(op, arg)
  }
}

class Binary(name: String) extends ((Expr, Expr) => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(Apply(`op`, arg1), arg2) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr) = {
    Apply(Apply(op, arg1), arg2)
  }
}

class Ternary(name: String) extends ((Expr, Expr, Expr) => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(Apply(Apply(`op`, arg1), arg2), arg3) =>
      Some((arg1, arg2, arg3))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr, arg3: Expr) = {
    Apply(Apply(Apply(op, arg1), arg2), arg3)
  }
}