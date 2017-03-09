package ulang

import arse._

trait Pretty {
  override def toString = printer.print(this)
}

object printer {
  def print_number(n: Int, any: Any): String = any match {
    case Obj(Tag("+1"), List(arg)) =>
      print_number(n + 1, arg)
    case Tag("0") =>
      n + ""
    case _ =>
      any + " + " + n
  }

  def print_tuple(any: Any): String = any match {
    case App(Tag(","), List(arg1, arg2)) =>
      ", " + arg1 + print_tuple(arg2)
    case UnApp(Tag(","), List(arg1, arg2)) =>
      ", " + arg1 + print_tuple(arg2)
    case Obj(Tag(","), List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case _ =>
      ", " + any + ")"
  }

  def print_list(any: Any): String = any match {
    case App(Tag("::"), List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case UnApp(Tag("::"), List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case Obj(Tag("::"), List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case Tag("[]") =>
      "]"
    case _ =>
      "] ++ " + any
  }

  def print(any: Pretty): String = any match {
    case Wildcard =>
      "_"

    case Lit(s: String) =>
      "\"" + s + "\""
    case Lit(i: Int) =>
      i.toString
    case Lit(c: Char) =>
      "\'" + c + "\'"

    case Atom(name) if operators contains name =>
      "(" + name + ")"
    case Atom(name) =>
      name

    case SubPat(name, pat) =>
      name + " @ " + pat

    case UnApp(Tag(","), List(arg1, arg2)) =>
      "(" + arg1 + print_tuple(arg2)
    case UnApp(Tag("::"), List(arg1, arg2)) =>
      "{" + arg1 + print_list(arg2)
    case UnApp(Atom(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case UnApp(Atom(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case UnApp(Atom(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case UnApp(fun, args) =>
      (fun :: args).mkString("(", " ", ")")
    case Force(body) =>
      "$ " + body

    case App(Tag(","), List(arg1, arg2)) =>
      "(" + arg1 + print_tuple(arg2)
    case App(Tag("::"), List(arg1, arg2)) =>
      "[" + arg1 + print_list(arg2)
    case App(Atom(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case App(Atom(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case App(Atom(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case App(fun, args) =>
      (fun :: args).mkString("(", " ", ")")

    case Case(pats, None, body) =>
      pats.mkString(" ") + " -> " + body
    case Case(pats, Some(cond), body) =>
      pats.mkString(" ") + " if " + cond + " -> " + body

    case Bind(cases) =>
      "\\ " + cases.mkString(" | ")
    case Raise(args) =>
      args.mkString("raise ", ", ", "")
    case TryCatch(arg, cases) =>
      "try " + arg + " catch " + cases.mkString(" | ")
    case MatchWith(args, cases) =>
      "match " + args.mkString(" ") + " with " + cases.mkString(" | ")

    case LetEq(pat, arg) =>
      pat + " = " + arg
    case LetIn(eqs, body) =>
      "let " + eqs.mkString(", ") + " in " + body
    case IfThenElse(test, iftrue, iffalse) =>
      "if " + test + " then " + iftrue + " else " + iffalse
    case Susp(body) =>
      "$ " + body

    case Tok(str) =>
      "\"" + str + "\""
    case Match(pat: String) =>
      "\"" + pat + "\""
    case Alt(rules) =>
      rules.mkString("(", " | ", ")")
    case Rep(rule, plus) =>
      if (plus) rule + " +"
      else rule + " *"
    case Seq(rules, None) =>
      rules.mkString(" ")
    case Seq(rules, Some(action)) =>
      rules.mkString(" ") + " { " + action + " }"

    case Def(lhs, None, rhs) =>
      lhs + " = " + rhs + ";"
    case Def(lhs, Some(cond), rhs) =>
      lhs + " if " + cond + " = " + rhs + ";"
    case Test(phi) =>
      phi + ";"
    case Prod(lhs, rule) =>
      lhs + " = " + rule + ";"

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
    case Langs(names) =>
      names.mkString("import\n  ", " ", ";")
    case Nots(fixs) =>
      fixs.mkString("notation\n  ", "\n  ", "\nend\n")
    case Defs(defs) =>
      defs.mkString("define\n  ", "\n  ", "\nend\n")
    case Tests(tests) =>
      tests.mkString("test\n  ", "\n  ", "\nend\n")
    case Evals(exprs) =>
      exprs.mkString("eval\n  ", "\n  ", "\nend\n")
    case Grammar(prods) =>
      prods.mkString("grammar\n  ", "\n  ", "\nend\n")

    case Module(defs) =>
      defs.mkString("", "\n", "\n")

    case State(_, defs, prods) =>
      val s1 = defs.mkString("define\n  ", "\n  ", "\nend\n")
      val s2 = defs.mkString("grammar\n  ", "\n  ", "\nend\n")
      s1 + s2

    case Model(dyn) =>
      val lines = dyn.map { case (name, rhs) => name + " = " + rhs + ";" }
      lines.mkString("model\n  ", "\n  ", "\nend\n")
    case Parsers(ps) =>
      val lines = ps.map { case (name, rhs) => name + " = " + rhs + ";" }
      lines.mkString("parsers\n  ", "\n  ", "\nend\n")

    case Clos(cases, lex) =>
      "\\ " + cases.mkString(" | ") + lex.keys.mkString(" [", ", ", "]")
    case Prim(name, _) =>
      name
    case Obj(Tag("+1"), List(arg)) =>
      print_number(1, arg)
    case Obj(Tag(","), List(arg1, arg2: Eq)) =>
      "(" + arg1 + print_tuple(arg2)
    case Obj(Tag("::"), List(arg1, arg2: Eq)) =>
      "[" + arg1 + print_list(arg2)
    case Obj(Tag(name), List(arg)) if operators.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case Obj(Tag(name), List(arg)) if operators.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case Obj(Tag(name), List(arg1, arg2)) if operators.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Obj(Tag(name), args) =>
      (name :: args).mkString("(", " ", ")")
    case Exc(args) =>
      args.mkString("raise ", ", ", "")
    case lzy @ Lazy(body, lex) =>
      lzy.memo match {
        case Some(v) =>
          "$ " + v
        case None =>
          "$ " + body + lex.keys.mkString(" [", ", ", "]")
      }
  }
}