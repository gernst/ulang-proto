package ulang.shell

import scala.language.postfixOps

import arse._
import arse.implicits._
import sourcecode.Name
import ulang.prove.Rule

object grammar {
  import ulang.expr.grammar.names
  import ulang.expr.grammar.expr
  import ulang.expr.grammar.pat
  import ulang.prove.grammar.rule

  val left = Left("left")
  val right = Right("right")
  val assoc = left | right | ret(Non)

  val prefix = Prefix("prefix" ~ int)
  val postfix = Postfix("postfix" ~ int)
  val infix = Infix("infix" ~ assoc ~ int)

  val fixity = prefix | postfix | infix

  val fix = Fix(fixity ~ names)
  val data = Data("data" ~ names)

  val expr_high = expr above 7
  val cond_high = "if" ~ expr_high
  val pat_high = pat above 7

  val df = Def(pat_high ~ (cond_high ?) ~ "=" ~ expr)

  def proof = "proof" ~ rule ~ ";"
  val prop = Thm(expr ~ ";" ~ (proof ?))

  val test = Test(expr)

  val def_ = (df ~ ";") *
  val test_ = (test ~ ";") *
  val notation_ = ((fix | data) ~ ";") *
  val expr_ = (expr ~ ";") *
  val prop_ = prop *

  val imports = Imports("import" ~ names ~ ";")
  val defs = Defs("definition" ~ def_ ~ "end")
  val tests = Tests("test" ~ test_ ~ "end")
  val notations = Notations("notation" ~ notation_ ~ "end")
  val evals = Evals("eval" ~ expr_ ~ "end")
  val props = Thms("theorem" ~ prop_ ~ "end")

  val cmd = imports | notations | defs | tests | evals | props;

  val module = Module(cmd *)
}


