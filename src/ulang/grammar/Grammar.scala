package ulang.grammar

import scala.language.postfixOps

import arse._

import ulang._

trait Grammar {
  import arse.Parser._
  import arse.Recognizer._
  
  import core.Grammar._
  
  val attribute = "{" ~ expr ~ "}"
  
  val production = closed *
  
  val lhs = expr ~ expect("::=")
  val rule = Rule.from(lhs, production, attribute ?)
}