package ulang.test

import arse._
import tst.Test

object parser extends Test {
  val expr_eof = ulang.expr.grammar.expr.$

  implicit object W extends Whitespace("\\s*")
  
  def parse(text: String) = {
    expr_eof parse text
  }

  test("basic") {
    println(parse("\\x, y -> x y \\y z -> x y z"))
    println(parse("let x = x, y = x in (x, y)"))
  }

  def main(args: Array[String]) {
  }
}