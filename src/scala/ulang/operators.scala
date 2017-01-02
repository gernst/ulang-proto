package ulang

import arse._

object operators extends Syntax[String] {
  var data: Set[String] = Set()
  var prefix_ops: Map[String, Int] = Map()
  var postfix_ops: Map[String, Int] = Map()
  var infix_ops: Map[String, (Assoc, Int)] = Map()
}