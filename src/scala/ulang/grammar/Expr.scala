package ulang.grammar

import ulang._

sealed trait Closed

case class Id(name: String) extends Closed {
  override def toString = name
}

case class Rec(alt: Alt) extends Closed {
  override def toString = "(" + alt + ")"
}

case class Rep(arg: Closed, how: Option[String]) {
  override def toString = how match {
    case Some(how) => arg + " " +how
    case None => "" + arg
  }
}

case class Seq(args: List[Rep]) {
  override def toString = args.mkString(" ")
}

case class Attr(arg: Seq, fun: Option[core.Expr]) {
  override def toString = fun match {
    case Some(fun) => arg + " { " + fun + " }"
    case None => "" + arg
  }
}

case class Alt(args: List[Attr]) {
  override def toString = args.mkString(" | ")
}

case class Rule(lhs: Id, rhs: Alt) {
  override def toString = lhs + " ::= " + rhs
}