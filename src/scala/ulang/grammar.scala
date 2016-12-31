package ulang

import scala.language.postfixOps

import arse._

import ulang._

object grammar {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  val keywords = Set(";", "(", ")", "{", "}", ".", "->", "==", "|", "\\",
    "if", "then", "else", "let", "in", "match", "with", "end")

  val name = string filterNot keywords
  val nonmixfix = name filterNot operators.contains

  val expr: Parser[List[String], Expr] = mixfix(inner, Id, Apply, operators)
  val exprs = expr +
  
  val closed: Parser[List[String], Expr] = Parser.rec(parens(open) | fun | matches | ite | let | id)
  val closeds = closed +

  // grammar
  val id = Id.from(nonmixfix)
  val anyid = Id.from(name)

  def cases(dot: String) = {
    val dot_ = dot ~ expr
    val cs = Case.from(exprs, dot_)
    "|".? ~ cs.rep(sep = "|")
  }

  val fun = "\\" ~ Bind.from(cases("."))

  val if_ = "if" ~ expr
  val then_ = expect("then") ~ expr
  val else_ = expect("else") ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ closed
  val eq_ = expect("=") ~ expr
  val in_ = expect("in") ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val match_ = "match" ~ closeds
  val with_ = expect("with") ~ cases("->")
  val matches = Match.from(match_, with_)

  val open = expr | anyid

  val app = Apply.from(closed, closed +)
  val inner = app | closed

  val lhs = expr ~ expect("==")
  val rhs = expr ~ expect(";")
  val df = Def.from(lhs, rhs)
  val defs = df *
}


