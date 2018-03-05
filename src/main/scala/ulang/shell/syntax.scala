package ulang.shell

import ulang.Pretty
import arse.Fixity
import ulang.expr.Pat
import ulang.expr.Expr
import ulang.prove.Rule

sealed trait Notation extends Pretty
case class Data(names: List[String]) extends Notation
case class Fix(fixity: Fixity, names: List[String]) extends Notation

case class Def(lhs: Pat, cond: Option[Expr], rhs: Expr) extends Pretty
case class Test(phi: Expr) extends Pretty

case class Prop(phi: Expr, proof: Option[Rule]) extends Pretty

sealed trait Cmd extends Pretty
case class Imports(names: List[String]) extends Cmd
case class Notations(fixs: List[Notation]) extends Cmd
case class Defs(defs: List[Def]) extends Cmd
case class Tests(tests: List[Test]) extends Cmd
case class Evals(exprs: List[Expr]) extends Cmd
case class Props(props: List[Prop]) extends Cmd

case class Module(cmds: List[Cmd]) extends Pretty
