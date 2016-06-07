package ulang

import ulang.syntax._

trait Pretty {
  override def toString = Printer.pp(this)
}

object Printer {
  import Operators._

  def pp(any: Pretty): String = any match {
    case expr: Expr => pp(expr)
    case cmd: Cmd   => pp(cmd)
    case _          => sys.error("no pretty printer for object")
  }

  def pp(cmd: Cmd): String = cmd match {
    case Cmd(name, exprs) =>
      exprs mkString (name + "\n", ";\n", ";\n")
  }

  def pp(expr: Expr): String = expr match {
    case Op(name) =>
      "(" + name + ")"

    case Id(name) =>
      name

    case Applys(Id(name), List(arg)) if prefix_ops contains name =>
      "(" + name + " " + arg + ")"

    case Applys(Id(name), List(arg)) if postfix_ops contains name =>
      "(" + arg + " " + name + ")"

    case Applys(Id(name), List(arg1, arg2)) if infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"

    case Binds(Id(name), bounds, body) if bindfix_ops contains name =>
      "(" + name + " " + bounds.mkString(" ") + ". " + body + ")"

    case Lambdas(bounds, body) if !bounds.isEmpty =>
      "(lambda " + bounds.mkString(" ") + ". " + body + ")"

    case Applys(fun, args) if !args.isEmpty =>
      "(" + fun + " " + args.mkString(" ") + ")"
  }
}