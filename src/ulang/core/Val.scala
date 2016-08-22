package ulang.core

import ulang.core._

sealed trait Val
trait Data extends Val

case class Obj(data: Data, arg: Val) extends Data {
  override def toString = "(" + data + " " + arg + ")"
}

case class Closure(cases: List[Case], lex: Env) extends Val {
  override def toString = "<closure>"
}

case class Prim(apply: Val => Val) extends Val {
  override def toString = "<primitive>"
}