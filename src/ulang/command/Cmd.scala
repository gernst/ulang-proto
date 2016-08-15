package ulang.command

import ulang.Pretty
import ulang.syntax.Expr

case class Cmd(name: String, exprs: List[Expr]) extends Pretty {
}