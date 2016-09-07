package ulang.grammar

import scala.language.postfixOps

import arse._

import ulang._

case class Grammar(name: String, rules: List[Rule]) extends Source {
  override def toString = rules.mkString("grammar" + name + "\n", "\n", "\nend")
}

object Grammar extends ((String, List[Rule]) => Grammar) with Language {
  import arse.Parser._
  import arse.Recognizer._
  
  def build(parts: List[Source]) = {
    null
  }

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
  
  val parser = "grammar" ~ Grammar.from(string, rules) ~ "end"
}
