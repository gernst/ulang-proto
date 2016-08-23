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
  import arse.Mixfix._

  val keywords = Set(";", "(", ")", "{", "}", "::=", "*", "+", "|", "end")

  val name = string filterNot keywords
  val nonmixfix = name filterNot Operators.contains

  val core_expr = ulang.core.Grammar.expr

  val expr: Parser[List[String], Expr] = Parser.rec(Attr.from(seq, attr ?))

  val id = Id.from(nonmixfix)
  val attr = "{" ~ core_expr ~ "}"

  val closed = parens(expr) | id
  val inner = mixfix(closed, Id, Apply, Operators)
  val seq = Seqs.from(inner +)

  val lhs = expr ~ expect("::=")
  val rhs = expr ~ expect(";")
  val rule = Rule.from(lhs, rhs)
  val rules = rule *
}
