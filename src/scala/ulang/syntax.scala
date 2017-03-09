package ulang

import arse.Fixity

sealed trait Pat extends Pretty

sealed trait Expr extends Pretty
sealed trait Rule extends Pretty

case object Wildcard extends Pat

case class Lit(any: Any) extends Expr with Pat with Eq

sealed trait Atom extends Expr with Pat { def name: String }
case class Tag(name: String) extends Atom with Eq
case class Id(name: String) extends Atom with Rule

case class SubPat(name: String, pat: Pat) extends Pat

case class UnApp(fun: Pat, args: List[Pat]) extends Pat
case class App(fun: Expr, args: List[Expr]) extends Expr

case class Case(pats: List[Pat], cond: Option[Expr], body: Expr) extends Pretty
case class Bind(cases: List[Case]) extends Expr

case class Raise(args: List[Expr]) extends Expr
case class TryCatch(arg: Expr, cases: List[Case]) extends Expr

case class MatchWith(args: List[Expr], cases: List[Case]) extends Expr

case class LetEq(pat: Pat, arg: Expr) extends Pretty
case class LetIn(eqs: List[LetEq], body: Expr) extends Expr

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr

case class Force(body: Pat) extends Pat
case class Susp(body: Expr) extends Expr

case class Tok(str: String) extends Rule
case class Match(pat: String) extends Rule
case class Seq(rules: List[Rule]) extends Rule
case class Alt(rules: List[Rule]) extends Rule
case class Rep(rule: Rule, plus: Boolean) extends Rule
case class Attr(rule: Rule, action: Expr) extends Rule

sealed trait Not extends Pretty
case class Data(names: List[String]) extends Not
case class Fix(fixity: Fixity, names: List[String]) extends Not

case class Def(lhs: Pat, cond: Option[Expr], rhs: Expr) extends Pretty
case class Test(phi: Expr) extends Pretty

case class Prod(lhs: Id, rhs: Rule) extends Pretty

sealed trait Cmd extends Pretty
case class Imports(names: List[String]) extends Cmd
case class Nots(fixs: List[Not]) extends Cmd
case class Defs(defs: List[Def]) extends Cmd
case class Tests(tests: List[Test]) extends Cmd
case class Evals(exprs: List[Expr]) extends Cmd
case class Grammar(prods: List[Prod]) extends Cmd

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