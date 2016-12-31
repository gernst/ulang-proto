package ulang

sealed trait Expr

case class Id(name: String) extends Expr with Val {
  assert(!name.isEmpty)
  override def toString = this match {
    case Op(name) => "(" + name + ")"
    case _ => name
  }
}

case class Apply(fun: Expr, args: List[Expr]) extends Expr {
  override def toString = this match {
    case Apply(Op(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Apply(Op(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Apply(Op(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case _ =>
      (fun :: args).mkString("(", " ", ")")
  }
}

case class Case(pats: List[Expr], body: Expr) {
  override def toString = pats.mkString(" ") + ". " + body
}

case class Bind(cases: List[Case]) extends Expr {
  override def toString = "\\ " + cases.mkString(" | ")
}

case class Match(args: List[Expr], cases: List[Case]) extends Expr {
  override def toString = "match " + args.mkString(" ") + " with " + cases.mkString(" | ")
}

case class LetIn(pat: Expr, arg: Expr, body: Expr) extends Expr {
  override def toString = "let " + pat + " = " + arg + " in " + body
}

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr {
  override def toString = "if " + test + " then " + iftrue + " else " + iffalse
}

case class Def(lhs: Expr, rhs: Expr) {
  override def toString = lhs + " == " + rhs + ";"
}

case class Module(defs: List[Def]) {
  override def toString = defs.map(_ + ";\n").mkString
}

abstract class IdPred extends (String => Id) {
  def test(name: String): Boolean

  def apply(name: String) = {
    assert(test(name))
    Id(name)
  }

  def unapply(id: Id): Option[String] = id match {
    case Id(name) if test(name) =>
      Some(name)
    case _ =>
      None
  }
}

object Op extends IdPred {
  def test(str: String) = operators contains str
}

object Tag extends IdPred {
  def test(str: String) = str.head.isUpper || operators.constrs(str)
}