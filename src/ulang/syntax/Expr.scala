package ulang.syntax

import ulang._

case class Apply(fun: Expr, arg: Expr) extends Expr
case class Lambda(bound: Id, body: Expr) extends Expr

object Op {
  def unapply(expr: Expr): Option[String] = expr match {
    case Id(name) if Operators contains name =>
      Some(name)
    case _ =>
      None
  }
}

object Constr {
  def unapply(expr: Expr): Option[String] = expr match {
    case Id(name) if Character.isUpperCase(name.head) =>
      Some(name)
    case _ =>
      None
  }
}

object Bind extends ((Id, Id, Expr) => Expr) {
  def apply(op: Id, bound: Id, body: Expr): Expr = {
    Apply(op, Lambda(bound, body))
  }

  def unapply(expr: Expr): Option[(Id, Id, Expr)] = expr match {
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

object Lambdas extends ((List[Id], Expr) => Expr) {
  def apply(bounds: List[Id], body: Expr): Expr = {
    bounds.foldRight(body)(Lambda)
  }

  def unapply(expr: Expr): Option[(List[Id], Expr)] = {
    Some(flatten(expr))
  }

  def flatten(expr: Expr): (List[Id], Expr) = expr match {
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

  def unapply(expr: Expr): Option[(Id, List[Id], Expr)] = expr match {
    case Bind(op, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      Some((op, bound :: bounds, inner))
    case _ =>
      None
  }

  def flatten(op: Id, expr: Expr): (List[Id], Expr) = expr match {
    case Bind(`op`, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}