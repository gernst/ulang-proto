package ulang.grammar

import arse._

object Operators extends Syntax[String] {
  val prefix_ops: Map[String, Int] = Map()

  val postfix_ops: Map[String, Int] = Map(
    "*" -> 3,
    "+" -> 3)

  val infix_ops: Map[String, (Assoc, Int)] = Map(
    // "|" -> (Right, 1),
    "=" -> (Non, 2))
}