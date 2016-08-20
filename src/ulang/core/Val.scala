package ulang.core

import ulang.core._

sealed trait Val
case class Obj(tag: String, args: List[Val]) extends Val
case class Closure(cases: List[Case], lex: Env) extends Val