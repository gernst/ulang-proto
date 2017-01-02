package ulang

import arse._

trait Pretty {
  override def toString = printer.print(this)
}

object printer {
  def print(any: Pretty): String = any match {
    case Atom(name) if operators contains name =>
      "(" + name + ")"
    case Atom(name) =>
      name

    case Apply(Atom(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Apply(Atom(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Apply(Atom(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Apply(fun, args) =>
      (fun :: args).mkString("(", " ", ")")
    case Case(pats, body) =>
      pats.mkString(" ") + ". " + body
    case Bind(cases) =>
      "\\ " + cases.mkString(" | ")
    case Match(args, cases) =>
      "match " + args.mkString(" ") + " with " + cases.mkString(" | ")
    case LetIn(pat, arg, body) =>
      "let " + pat + " = " + arg + " in " + body
    case IfThenElse(test, iftrue, iffalse) =>
      "if " + test + " then " + iftrue + " else " + iffalse

    case Def(lhs, rhs) =>
      lhs + " == " + rhs + ";"

    case Data(names) =>
      "data " + names.mkString(" ") + ";"
    case Fix(Prefix(prec), names) =>
      "prefix " + prec + " " + names.mkString(" ") + ";"
    case Fix(Postfix(prec), names) =>
      "postfix " + prec + " " + names.mkString(" ") + ";"
    case Fix(Infix(Non, prec), names) =>
      "infix " + prec + " " + names.mkString(" ") + ";"
    case Fix(Infix(Left, prec), names) =>
      "infix left " + prec + " " + names.mkString(" ") + ";"
    case Fix(Infix(Right, prec), names) =>
      "infix right " + prec + " " + names.mkString(" ") + ";"

    case Imports(names) =>
      names.mkString("import\n  ", " ", ";")
    case Nots(fixs) =>
      fixs.mkString("notation\n  ", "\n  ", "\nend\n")
    case Defs(defs) =>
      defs.mkString("definitions\n  ", "\n  ", "\nend\n")
    case Evals(exprs) =>
      exprs.mkString("eval\n  ", "\n  ", "\nend\n")

    case Module(defs) =>
      defs.mkString("", "\n", "\n")

    case State(defs) =>
      defs.mkString("", "\n", "\n")

    case Model(dyn) =>
      val lines = dyn.map { case (name, rhs) => name + " == " + rhs + ";" }
      lines.mkString("model\n  ", "\n  ", "\nend\n")

    case Clos(cases, lex) =>
      "\\ " + cases.mkString(" | ") + lex.keys.mkString(" [", ", ", "]")
    case Prim(name, _) =>
      name
    case Obj(Tag(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Obj(Tag(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Obj(Tag(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Obj(Tag(name), args) =>
      (name :: args).mkString("(", " ", ")")
  }
}