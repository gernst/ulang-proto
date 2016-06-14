package ulang.command

import arse._

import ulang._
import ulang.syntax._

trait Handler extends (List[Expr] => Any) {
  // def parser: Parser[String, Cmd]
}