package ulang.prove

import arse._
import arse.implicits._

object grammar {
  val pat = ulang.expr.grammar.pat
  val expr = ulang.expr.grammar.expr

  val rule: Parser[Rule] = P(triv | ind)

  val cs = Case(pat ~ "->" ~ rule)
  val cases = ("|" ~ cs) *

  val triv = Trivial("trivial")
  val ind = Induction("induction" ~ expr ~ cases)
}