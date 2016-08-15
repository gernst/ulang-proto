package ulang.semantics

import ulang.Pretty
import ulang.syntax

sealed trait Val extends Pretty
case class Obj(tag: String, args: List[Val]) extends Val
case class Closure(cases: List[syntax.Case], lex: Env) extends Val