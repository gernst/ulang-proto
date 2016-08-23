package ulang.grammar

import ulang._

sealed trait Expr

case class Id(name: String) extends Expr {
  override def toString = name
}

case class Apply(id: Id, args: List[Expr]) extends Expr {
  override def toString = (id :: args).mkString("(", " ", ")")
}

case class Seq(args: List[Expr]) extends Expr {
  override def toString = args.mkString("(", " ", ")")
}

case class Attr(arg: Expr, fun: core.Expr) extends Expr {
  override def toString = arg + " { " + fun + " }"
}

object Seqs extends (List[Expr] => Expr) {
  def apply(args: List[Expr]) = {
    if(args.length == 1) args.head
    else Seq(args)
  }
}

object Attr extends ((Expr, Option[core.Expr]) => Expr) {
  def apply(arg: Expr, fun: Option[core.Expr]) = fun match {
    case Some(fun) => Attr(arg, fun)
    case None => arg
  }
}

case class Rule(lhs: Expr, rhs: Expr) {
  override def toString = lhs + " ::= " + rhs
}