package ulang

case class Obj(id: Id, args: List[Val]) {
  //  override def toString = "(" + data + " " + arg + ")"

  override def toString = this match {
    case Obj(Op(name), List(arg)) if Operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Obj(Op(name), List(arg)) if Operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Obj(Op(name), List(arg1, arg2)) if Operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case _ =>
      (id :: args).mkString("(", " ", ")")
  }
}