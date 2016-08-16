package ulang.syntax

import scala.language.postfixOps

import java.io.Reader
import java.io.File
import java.io.FileReader

import scala.collection.mutable.ListBuffer

import arse._

import ulang._
import ulang.command.Cmd

object Parser {

  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  import Operators._

  val keywords = Set("definitions", "inductive", "axioms", "lemmas", "theorems")
  val special = Set(";", ".", "(", ")", "=", "lambda", "if", "then", "else", "let", "in") ++ keywords

  val name = __ filterNot special
  val keyword = __ filter keywords
  val nonmixfix = name filterNot Operators.contains

  val regular_name = nonmixfix filterNot (_.isConstr)
  val any_name = name filterNot (_.isConstr)
  val constr_name = nonmixfix filter (_.isConstr)

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")

  val expr: Parser[List[String], Expr] = mixfix(app, Id, Applys, Operators)

  // grammar
  val const = Const.from(constr_name)
  val id = Id.from(regular_name)
  val anyid = Id.from(any_name)

  val ids_dot = id.+ ~ "."
  val binder = Binds.from(bindfix_op, ids_dot, expr)
  val lambda = "lambda" ~ Lambdas.from(ids_dot, expr)

  val if_ = "if" ~ expr
  val then_ = "then" ~ expr
  val else_ = "else" ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ expr
  val eq_ = "=" ~ expr
  val in_ = "in" ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val closed = parens(expr | anyid) | lambda | binder | ite | let | const | id

  val constr_app = Constr.from(constr_name, closed *)
  val regular_app = Applys.from(closed, closed *)
  
  val app = constr_app | regular_app

  val expr_eol = expr ~ ";"
  val exprs = expr_eol *
  val command = Cmd.from(keyword, exprs)
  val commands = command *

  def load(file: File) = {
    val in = tokenize(new FileReader(file))
    val (res, Nil) = commands(in)
    res
  }

  def tokenize(reader: Reader) = {
    val scanner = new Scanner(reader)
    val res = new ListBuffer[String]()
    var tok = scanner.next()
    while (tok != null) {
      res += tok
      tok = scanner.next()
    }
    res.toList
  }
}


