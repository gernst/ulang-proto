package ulang

import arse.Fixity

sealed trait Expr extends Pretty

sealed trait Atom extends Expr { def name: String }
case class Tag(name: String) extends Atom with Val
case class Id(name: String) extends Atom

case class App(fun: Expr, args: List[Expr]) extends Expr

case class Case(pats: List[Expr], body: Expr) extends Pretty
case class Bind(cases: List[Case]) extends Expr

case class Match(args: List[Expr], cases: List[Case]) extends Expr
case class LetIn(pat: Expr, arg: Expr, body: Expr) extends Expr
case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr

sealed trait Not extends Pretty
case class Data(names: List[String]) extends Not
case class Fix(fixity: Fixity, names: List[String]) extends Not

case class Def(lhs: Expr, rhs: Expr) extends Pretty

sealed trait Cmd extends Pretty
case class Imports(names: List[String]) extends Cmd
case class Nots(fixs: List[Not]) extends Cmd
case class Defs(defs: List[Def]) extends Cmd
case class Evals(exprs: List[Expr]) extends Cmd

case class Module(cmds: List[Cmd]) extends Pretty {
  def ++(that: Module) = Module(this.cmds ++ that.cmds)
}

object Atom extends (String => Expr) {
  def isTag(name: String) = {
    name.head.isUpper || (operators.data contains name)
  }

  def apply(name: String) = {
    if (isTag(name)) Tag(name)
    else Id(name)
  }

  def unapply(atom: Atom) = {
    Some(atom.name)
  }
}