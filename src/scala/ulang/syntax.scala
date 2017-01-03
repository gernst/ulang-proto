package ulang

import arse.Fixity

sealed trait Pat extends Pretty

case object Wildcard extends Pat

sealed trait Expr extends Pretty

sealed trait Atom extends Expr with Pat { def name: String }
case class Tag(name: String) extends Atom with Val
case class Id(name: String) extends Atom

case class UnApp(fun: Pat, args: List[Pat]) extends Pat
case class App(fun: Expr, args: List[Expr]) extends Expr

case class Case(pats: List[Pat], cond: Option[Expr], body: Expr) extends Pretty
case class Bind(cases: List[Case]) extends Expr

case class Match(args: List[Expr], cases: List[Case]) extends Expr
case class LetIn(pat: Pat, arg: Expr, body: Expr) extends Expr
case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr

case class Force(body: Pat) extends Pat
case class Susp(body: Expr) extends Expr

sealed trait Not extends Pretty
case class Data(names: List[String]) extends Not
case class Fix(fixity: Fixity, names: List[String]) extends Not

case class Def(lhs: Pat, cond: Option[Expr], rhs: Expr) extends Pretty
case class Test(lhs: Expr, rhs: Expr) extends Pretty

sealed trait Cmd extends Pretty
case class Imports(names: List[String]) extends Cmd
case class Nots(fixs: List[Not]) extends Cmd
case class Pats(pats: List[Def]) extends Cmd
case class Defs(defs: List[Def]) extends Cmd
case class Tests(tests: List[Test]) extends Cmd
case class Evals(exprs: List[Expr]) extends Cmd

case class Module(cmds: List[Cmd]) extends Pretty {
  def ++(that: Module) = Module(this.cmds ++ that.cmds)
}

object Atom extends (String => Atom) {
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