package ulang.prove

import arse._
import arse.implicits._

object grammar {
  val expr = ulang.expr.grammar.expr

  val triv = Trivial("trivial")
  val ind = Induction("induction" ~ expr ~ ret(Nil))
  val rule = triv | ind
}