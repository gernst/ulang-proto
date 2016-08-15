package ulang

import arse._

object Operators extends Syntax[String] {
  def contains(s: String) =
    (prefix_ops contains s) ||
      (postfix_ops contains s) ||
      (infix_ops contains s) ||
      (bindfix_ops contains s)

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
    ":" -> (Right, 8),
    "++" -> (Right, 7),
    "!=" -> (Non, 6),
    "==" -> (Non, 6),
    "<=" -> (Non, 6),
    ">=" -> (Non, 6),
    "<" -> (Non, 6),
    ">" -> (Non, 6),
    "->" -> (Right, 6),
    "::" -> (Non, 5),
    "and" -> (Left, 4),
    "or" -> (Left, 3),
    "==>" -> (Right, 2),
    "<==>" -> (Non, 1),
    "," -> (Left, 0))

  val bindfix_ops: Set[String] = Set(
    "exists",
    "forall",
    "choose")

  val bindfix_op = Mixfix.mixfix_op(bindfix_ops, Id)
}