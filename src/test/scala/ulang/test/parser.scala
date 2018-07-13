package ulang.test

import arse._
import tst.Test
import ulang.expr.Free
import ulang.expr.UnApp
import ulang.expr.Pat
import ulang.expr.SubPat

object parser extends Test {
  val expr_eof = ulang.expr.grammar.expr.$

  implicit object W extends Whitespace("\\s*")

  def parse(text: String) = {
    expr_eof parse text
  }

  def main(args: Array[String]) {
    println(parse("\\x -> \\y -> x y"))
    println(parse("\\x, y -> x y"))
    println(parse("\\x, y -> x y \\y z -> x y z"))
    println(parse("\\(C x) as x, x -> x"))
    
    val pats = List(Free("x"), UnApp(Free("z"), Free("y")), SubPat(Free("w"), UnApp(Free("x"), Free("w"))), Free("y"), Free("z"), Free("w"))
    println(Pat.bind(pats, Nil, 0))
  }
}