package ulang.core

import ulang.core._

trait Data extends Val

case class Closure(cases: List[Case], lex: Env) {
  override def toString = "closure " + cases.mkString(" ") + lex.keys.mkString(" [", ", ", "]")
}

case class Obj(data: Data, arg: Val) extends Data {
  //  override def toString = "(" + data + " " + arg + ")"

  override def toString = this match {
    case Objs(Op(name), List(arg)) if Operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Objs(Op(name), List(arg)) if Operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Objs(Op(name), List(arg1, arg2)) if Operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Objs(fun, args) =>
      (fun :: args).mkString("(", " ", ")")
  }
}

object Objs extends ((Data, List[Val]) => Val) {
  def apply(fun: Data, args: List[Val]) = {
    args.foldLeft(fun)(Obj)
  }

  def unapply(expr: Val) = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Val, args: List[Val]): (Val, List[Val]) = expr match {
    case Obj(fun, arg) => flatten(fun, arg :: args)
    case _ => (expr, args)
  }
}