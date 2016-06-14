package ulang

trait Pretty {
  override def toString = Printer.pp("", this)
}

object Printer {
  import Operators._

  def pp(any: Pretty): String = {
    pp("", any)
  }

  def pp(indent: String, any: Pretty): String = any match {
    case cmd: Cmd     => pp(indent, cmd)
    case expr: Expr   => pp(indent, expr)
    case data: Data   => pp(indent, data)
    case proof: Proof => pp(indent, proof)
    case _            => sys.error("no pretty printer for object")
  }

  def pp(indent: String, cmd: Cmd): String = cmd match {
    case Cmd(name, exprs) =>
      var res = ""
      res += indent + name + "\n"
      for (expr <- exprs) {
        res += pp(indent, expr) + ";\n"
      }
      res
  }

  def pp(indent: String, proof: Proof): String = proof match {
    case seq: calculus.Seq =>
      indent + seq.ant.mkString(", ") + " |- " + seq.suc.mkString(", ")

    case calculus.Step(prems, concl, rule) =>
      var res = ""
      res += pp(indent, concl) + " by " + rule
      for (prem <- prems) {
        res += pp("\n" + indent + "  ", prem)
      }
      res
  }

  def pp(indent: String, data: Data): String = data match {
    case Op(name) =>
      indent + "(" + name + ")"

    case Id(name) =>
      indent + name

    case semantics.Applys(Id(name), List(arg)) if prefix_ops contains name =>
      indent + "(" + name + " " + arg + ")"

    case semantics.Applys(Id(name), List(arg)) if postfix_ops contains name =>
      indent + "(" + arg + " " + name + ")"

    case semantics.Applys(Id(name), List(arg1, arg2)) if infix_ops contains name =>
      indent + "(" + arg1 + " " + name + " " + arg2 + ")"

    case semantics.Applys(fun, args) if !args.isEmpty =>
      indent + "(" + fun + " " + args.mkString(" ") + ")"
  }

  def pp(indent: String, expr: Expr): String = expr match {
    case Op(name) =>
      indent + "(" + name + ")"

    case Id(name) =>
      indent + name

    case syntax.IfThenElse(test, arg1, arg2) =>
      indent + "(if " + test + " then " + arg1 + " else " + arg2 + ")"

    case syntax.Applys(Id(name), List(arg)) if prefix_ops contains name =>
      indent + "(" + name + " " + arg + ")"

    case syntax.Applys(Id(name), List(arg)) if postfix_ops contains name =>
      indent + "(" + arg + " " + name + ")"

    case syntax.Applys(Id(name), List(arg1, arg2)) if infix_ops contains name =>
      indent + "(" + arg1 + " " + name + " " + arg2 + ")"

    case syntax.Binds(Id(name), bounds, body) if bindfix_ops contains name =>
      indent + "(" + name + " " + bounds.mkString(" ") + ". " + body + ")"

    case syntax.Lambdas(bounds, body) if !bounds.isEmpty =>
      indent + "(lambda " + bounds.mkString(" ") + ". " + body + ")"

    case syntax.Match(cases) =>
      val cs = cases.map {
        case syntax.Case(pattern, body) =>
          pattern + ". " + body
      }
      indent + cs.mkString("(lambda ", " | ", ")")

    case syntax.Applys(fun, args) if !args.isEmpty =>
      indent + "(" + fun + " " + args.mkString(" ") + ")"
  }
}