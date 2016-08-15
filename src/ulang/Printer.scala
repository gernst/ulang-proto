package ulang

import ulang._
import ulang.command._
import ulang.syntax._
import ulang.semantics._

trait Pretty {
  override def toString = Printer.pp("", this)
}

object Printer {
  import Operators._

  def pp(any: Pretty): String = {
    pp("", any)
  }

  def pp(indent: String, any: Pretty): String = any match {
    case cmd: Cmd => pp(indent, cmd)
    case expr: Expr => pp(indent, expr)
    case obj: Val => pp(indent, obj)
    case _ => sys.error("no pretty printer for object")
  }

  def pp(indent: String, obj: Val): String = obj match {
    case Obj(tag, Nil) =>
      indent + tag

    case Obj(tag, args) =>
      indent + "(" + tag + args.mkString(" ", " ", ")")

    case Closure(cases, env) =>
      val cs = pp(cases)
      val es = env.map {
        case (x, v) =>
          x + " == " + v
      }
      if (env.isEmpty) {
        indent + cs.mkString("(closure ", " | ", ")")
      } else {
        indent + cs.mkString("(closure ", " | ", " where ") + es.mkString(" ", ", ", ")")
      }
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

  /*
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
  */

  def pp(cases: List[Case]): List[String] = {
    cases.map {
      case Case(pattern, body) =>
        pattern + ". " + body
    }
  }

  def pp(indent: String, expr: Expr): String = expr match {
    case Op(name) =>
      indent + "(" + name + ")"

    case Id(name) =>
      indent + name
      
    case Const(tag) =>
      indent + tag

    case IfThenElse(test, arg1, arg2) =>
      indent + "(if " + test + " then " + arg1 + " else " + arg2 + ")"

    case Applys(Id(name), List(arg)) if prefix_ops contains name =>
      indent + "(" + name + " " + arg + ")"

    case Applys(Id(name), List(arg)) if postfix_ops contains name =>
      indent + "(" + arg + " " + name + ")"

    case Applys(Id(name), List(arg1, arg2)) if infix_ops contains name =>
      indent + "(" + arg1 + " " + name + " " + arg2 + ")"

    case Binds(Id(name), bounds, body) if bindfix_ops contains name =>
      indent + "(" + name + " " + bounds.mkString(" ") + ". " + body + ")"

    case Lambdas(bounds, body) if !bounds.isEmpty =>
      indent + "(lambda " + bounds.mkString(" ") + ". " + body + ")"

    case Match(cases) =>
      val cs = pp(cases)
      indent + cs.mkString("(lambda ", " | ", ")")
      
    case Constr(tag, args) =>
      indent + "(" + tag + " " + args.mkString(" ") + ")"

    case Applys(fun, args) =>
      assert(!args.isEmpty)
      indent + "(" + fun + " " + args.mkString(" ") + ")"
  }
}