package ulang.prove

import scala.language.postfixOps

import arse._
import arse.implicits._

object grammar {
  val pat = ulang.expr.grammar.pat
  val expr = ulang.expr.grammar.expr

  val rule: Parser[Rule] = P(triv | sorry | ind)

  val cs = Case(pat ~ "->" ~ rule)
  val cases = ("|" ~ cs) *

  val triv = Trivial("trivial")
  val sorry = Sorry("sorry")
  val ind = Induction("induction" ~ expr ~ cases)
}