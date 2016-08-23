package ulang.core

import scala.language.postfixOps

import arse._

import ulang._

object Grammar {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  val keywords = Set(";", "(", ")", "{", "}", ".", "->", "==", "|", "\\",
    "function", "if", "then", "else", "let", "in", "match", "with", "end")

  val name = string filterNot keywords
  val nonmixfix = name filterNot Operators.contains

  val expr: Parser[List[String], Expr] = mixfix(inner, Id, Applys, Operators)
  val exprs = expr +

  // grammar
  val id = Id.from(nonmixfix)
  val anyid = Id.from(name)

  val dot_ = "." ~ expr
  val arrow_ = "->" ~ expr
  val cs = Case.from(expr, arrow_)
  val cases = "|".? ~ cs.rep(sep = "|")

  val bind = Lambdas.from(Parser.rec(closed +), arrow_)
  val binds = "|".? ~ bind.rep(sep = "|")
  val function = "function" ~ Merge.from(binds)

  val if_ = "if" ~ expr
  val then_ = expect("then") ~ expr
  val else_ = expect("else") ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ Parser.rec(closed)
  val eq_ = expect("=") ~ expr
  val in_ = expect("in") ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val match_ = "match" ~ expr
  val with_ = expect("with") ~ cases
  val matches = Match.from(match_, with_)

  val open = expr | anyid
  val closed: Parser[List[String], Expr] = parens(open) | function | matches | ite | let | id

  val app = Applys.from(closed, closed *)
  val inner = app

  val lhs = expr ~ expect("==")
  val rhs = expr ~ expect(";")
  val df = Def.from(lhs, rhs)
  val defs = df *
}


