package ulang.core

sealed trait Expr

case class Id(name: String) extends Expr with Data {
  assert(!name.isEmpty)
  override def toString = this match {
    case Op(name) => "(" + name + ")"
    case _ => name
  }
}

case class Apply(fun: Expr, arg: Expr) extends Expr {
  override def toString = this match {
    case Applys(Op(name), List(arg)) if Operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Applys(Op(name), List(arg)) if Operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Applys(Op(name), List(arg1, arg2)) if Operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Applys(fun, args) =>
      (fun :: args).mkString("(", " ", ")")
  }
}

case class Case(pat: Expr, body: Expr) {
  override def toString = pat + ". " + body
}

case class Bind(cases: List[Case]) extends Expr {
  override def toString = "lambda " + cases.mkString(" | ")
}

case class Match(arg: Expr, cases: List[Case]) extends Expr {
  override def toString = "match " + arg + " with " + cases.mkString(" | ")
}

case class LetIn(pat: Expr, arg: Expr, body: Expr) extends Expr {
  override def toString = "let " + pat + " = " + arg + " in " + body
}

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr {
  override def toString = "if " + test + " then " + iftrue + " else " + iffalse
}

case class Def(lhs: Expr, rhs: Expr) {
  override def toString = lhs + " == " + rhs
}