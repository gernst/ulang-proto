package ulang.core

import arse._

object Operators extends Syntax[String] {
  val constrs = Set("::", "+1", ",", "[]")

  val prefix_ops: Map[String, Int] = Map(
    "not" -> 5)

  val postfix_ops: Map[String, Int] = Map(
    "+1" -> 11,
    "-1" -> 11)

  val infix_ops: Map[String, (Assoc, Int)] = Map(
    "*" -> (Left, 9),
    "/" -> (Left, 9),
    "+" -> (Left, 8),
    "-" -> (Left, 8),
    "::" -> (Right, 8),
    "++" -> (Right, 7),
    "!=" -> (Non, 6),
    "=" -> (Non, 6),
    "<=" -> (Non, 6),
    ">=" -> (Non, 6),
    "<" -> (Non, 6),
    ">" -> (Non, 6),
    // "->" -> (Right, 6),
    "and" -> (Left, 4),
    "or" -> (Left, 3),
    "==>" -> (Right, 2),
    "<==>" -> (Non, 1),
    "," -> (Right, 0))
}