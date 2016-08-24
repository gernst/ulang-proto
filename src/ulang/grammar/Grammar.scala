package ulang.grammar

import scala.language.postfixOps

import arse._

import ulang._

case class Grammar(rules: List[Rule]) extends Language {
  import arse.Parser._
  import arse.Recognizer._

  override def toString = rules.mkString("grammar\n", "\n", "\nend")

  def extend(add: List[Rule]) = Grammar(rules ++ add)
  val parser = (extend _).from("grammar" ~ Grammar.rules ~ "end")
}

object Grammar {
  import arse.Parser._
  import arse.Recognizer._

  val keywords = Set(";", "(", ")", "{", "}", "::=", "*", "+", "|", "end")

  val name = string filterNot keywords
  val nonmixfix = name filterNot Operators.contains

  val core_expr = ulang.core.Grammar.expr

  val expr: Parser[List[String], Alt] = Parser.rec(alt)

  val id = Id.from(nonmixfix)
  val code = "{" ~ core_expr ~ "}"

  val closed = Rec.from(parens(expr)) | id
  val un = lit("*", "*") | lit("+", "+")
  val rep = Rep.from(closed, un ?)
  val seq = Seq.from(rep +)
  val attr = Attr.from(seq, code ?)
  val alt = Alt.from(attr.rep(sep = "|"))

  val lhs = id ~ expect("::=")
  val rhs = expr ~ expect(";")
  val rule = Rule.from(lhs, rhs)
  val rules = rule *
}
