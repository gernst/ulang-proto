package ulang.semantics

import ulang._

case class Apply(data: Data, arg: Val) extends Data
case class Closure(bound: Id, body: Expr, lex: Env) extends Data