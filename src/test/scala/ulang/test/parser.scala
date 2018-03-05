package ulang.test

import arse._
import tst.Test

object parser extends Test {
  val expr_eof = ulang.expr.grammar.pat.$

  implicit object W extends Whitespace("\\s*")
  
  def parse(text: String) = {
    expr_eof parse text
  }

  test("basic") {
    println(parse("_"))
  }

  def main(args: Array[String]) {
  }
}