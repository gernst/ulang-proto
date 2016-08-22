package ulang.core

import scala.language.postfixOps

import arse._

object Grammar {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  val keywords = Set(";", ".", "(", ")", "==", "|", "lambda", "if", "then", "else", "let", "in", "match", "with", "end")

  val name = string filterNot keywords
  val nonmixfix = name filterNot Operators.contains

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")
  
  val expr: Parser[List[String], Expr] = mixfix(inner, Id, Applys, Operators)
  val exprs = expr +

  // grammar
  val id = Id.from(nonmixfix)
  val anyid = Id.from(name)

  val dot_ = "." ~ expr
  val cs = Case.from(expr, dot_)
  val cases = "|".? ~ cs.rep(sep = "|")

  val bind = Lambdas.from(Parser.rec(closed +), dot_)
  val binds = bind.rep(sep = "|")
  val lambda = "lambda" ~ Merge.from(binds)

  val if_ = "if" ~ expr
  val then_ = "then" ~ expr
  val else_ = "else" ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ expr
  val eq_ = "=" ~ expr
  val in_ = "in" ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val match_ = "match" ~ expr
  val with_ = "with" ~ "|".? ~ cases
  val matches = Match.from(match_, with_)

  val open = expr | anyid
  val closed: Parser[List[String], Expr] = parens(open) | lambda | ite | let | id

  val app = Applys.from(closed, closed *)
  val inner = app

  val lhs = expr ~ expect("==")
  val rhs = expr ~ expect(";")
  val df = Def.from(lhs, rhs)
  val defs = df *
}


