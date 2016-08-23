package ulang.core

import ulang.core._

trait Data extends Val

case class Obj(data: Data, arg: Val) extends Data {
  override def toString = "(" + data + " " + arg + ")"
}

case class Closure(cases: List[Case], lex: Env) {
  override def toString = "closure " + cases.mkString(" ") + lex.keys.mkString(" [", ", ", "]")
}
