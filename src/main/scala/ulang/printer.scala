package ulang

import arse.Infix
import arse.Left
import arse.Non
import arse.Postfix
import arse.Prefix
import arse.Right
import ulang.expr.App
import ulang.expr.Atom
import ulang.expr.Case
import ulang.expr.Clos
import ulang.expr.Eq
import ulang.expr.IfThenElse
import ulang.expr.Lambda
import ulang.expr.LetEq
import ulang.expr.LetIn
import ulang.expr.Lit
import ulang.expr.MatchWith
import ulang.expr.Obj
import ulang.expr.SubPat
import ulang.expr.Tag
import ulang.expr.UnApp
import ulang.expr.Wildcard
import ulang.expr.builtin
import ulang.expr.operators
import ulang.prove.Derivation
import ulang.prove.Goal
import ulang.prove.Step
import ulang.shell.Data
import ulang.shell.Def
import ulang.shell.Defs
import ulang.shell.Evals
import ulang.shell.Fix
import ulang.shell.Imports
import ulang.shell.Ind
import ulang.shell.Notations
import ulang.shell.Test
import ulang.shell.Tests

trait Pretty {
  override def toString = printer.print(this)
}

object printer {
  def print_number(n: Int, any: Any): String = any match {
    case Obj(builtin.Succ, List(arg)) =>
      print_number(n + 1, arg)
    case builtin.Zero =>
      n + ""
    case _ =>
      any + " + " + n
  }

  def print_list(any: Any): String = any match {
    case App(builtin.Cons, List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case UnApp(builtin.Cons, List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case Obj(builtin.Cons, List(arg1, arg2)) =>
      ", " + arg1 + print_list(arg2)
    case builtin.Nil =>
      "]"
    case _ =>
      "; " + any + "]"
  }

  def print_derivation(deriv: Derivation, indent: Int = 0): String = {
    val sp = "  " * indent

    deriv match {
      case Step(prems, concl, rule) =>
        var res = sp
        res += print_derivation(concl, indent) + " by " + rule
        if (!prems.isEmpty) res += "\n"
        for (prem <- prems) {
          res += print_derivation(prem, indent + 1)
          res += "\n"
        }
        res
      case Goal(Nil, Nil, assert) =>
        sp + "|- " + assert
      case Goal(eqs, ant, suc) =>
        var res = sp
        val s1 = eqs.map { case (lhs, rhs) => lhs + " = " + rhs }
        val s2 = ant.map { _.toString }
        res += (s1 ++ s2).mkString(", ")
        res += " |- " + suc
        res
    }
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

    case builtin.Nil =>
      "[]"
    case op: Atom if operators contains op =>
      "(" + op.name + ")"
    case op: Atom =>
      op.name

    case SubPat(name, pat) =>
      name + " @ " + pat

    case UnApp(builtin.Tuple, args) =>
      args.mkString("(", ", ", ")")
    case UnApp(builtin.Cons, List(arg1, arg2)) =>
      "[" + arg1 + print_list(arg2)
    case UnApp(op @ Atom(name), List(arg)) if operators.prefix_ops contains op =>
      "(" + name + " " + arg + ")"
    case UnApp(op @ Atom(name), List(arg)) if operators.postfix_ops contains op =>
      "(" + arg + " " + name + ")"
    case UnApp(op @ Atom(name), List(arg1, arg2)) if operators.infix_ops contains op =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case UnApp(fun, args) =>
      (fun :: args).mkString("(", " ", ")")

    case App(builtin.Tuple, args) =>
      args.mkString("(", ", ", ")")
    case App(builtin.Cons, List(arg1, arg2)) =>
      "[" + arg1 + print_list(arg2)
    case App(op @ Atom(name), List(arg)) if operators.prefix_ops contains op =>
      "(" + name + " " + arg + ")"
    case App(op @ Atom(name), List(arg)) if operators.postfix_ops contains op =>
      "(" + arg + " " + name + ")"
    case App(op @ Atom(name), List(arg1, arg2)) if operators.infix_ops contains op =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case App(fun, args) =>
      (fun :: args).mkString("(", " ", ")")

    case Case(pats, None, body) =>
      pats.mkString(" ") + " -> " + body
    case Case(pats, Some(cond), body) =>
      pats.mkString(" ") + " if " + cond + " -> " + body

    case Lambda(cases) =>
      "\\ " + cases.mkString(" | ")
    case MatchWith(args, cases) =>
      "match " + args.mkString(" ") + " with " + cases.mkString(" | ")

    case LetEq(pat, arg) =>
      pat + " = " + arg
    case LetIn(eqs, body) =>
      "let " + eqs.mkString(", ") + " in " + body
    case IfThenElse(test, iftrue, iffalse) =>
      "if " + test + " then " + iftrue + " else " + iffalse

    case Def(lhs, None, rhs) =>
      lhs + " = " + rhs + ";"
    case Def(lhs, Some(cond), rhs) =>
      lhs + " if " + cond + " = " + rhs + ";"
    case Test(phi) =>
      phi + ";"

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
    case Notations(fixs) =>
      fixs.mkString("notation\n  ", "\n  ", "\nend\n")
    case Defs(defs) =>
      defs.mkString("definition\n  ", "\n  ", "\nend\n")
    case Tests(tests) =>
      tests.mkString("test\n  ", "\n  ", "\nend\n")
    case Evals(exprs) =>
      exprs.mkString("eval\n  ", "\n  ", "\nend\n")
    case Ind(cases) =>
      cases.mkString("inductive definition\n  ", "\n  ", "\nend\n")

    case Clos(cases, lex) =>
      "\\ " + cases.mkString(" | ") + lex.keys.mkString(" [", ", ", "]")
    case Obj(builtin.Succ, List(arg)) =>
      print_number(1, arg)
    case Obj(builtin.Tuple, args) =>
      args.mkString("(", ", ", ")")
    case Obj(builtin.Cons, List(arg1, arg2: Eq)) =>
      "[" + arg1 + print_list(arg2)
    case Obj(op: Tag, List(arg)) if operators.prefix_ops contains op =>
      "(" + op + " " + arg + ")"
    case Obj(op: Tag, List(arg)) if operators.postfix_ops contains op =>
      "(" + arg + " " + op + ")"
    case Obj(op: Tag, List(arg1, arg2)) if operators.infix_ops contains op =>
      "(" + arg1 + " " + op + " " + arg2 + ")"
    case Obj(op: Tag, args) =>
      (op :: args).mkString("(", " ", ")")

    case deriv: Derivation =>
      print_derivation(deriv)
  }
}