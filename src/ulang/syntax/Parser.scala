package ulang.syntax

import arse._
import java.io.Reader
import java.io.File
import java.io.FileReader
import scala.collection.mutable.ListBuffer

import ulang._

object Parser {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._
  import ulang.Operators._

  val keywords = Set("definitions", "inductive", "axioms", "lemmas", "theorems")
  val special = Set(";", ".", "(", ")", "=", "lambda", "if", "then", "else", "let", "in") ++ keywords

  val name = __ filterNot special
  val keyword = __ filter keywords
  val nonmixfix = name filterNot Operators.contains
  def expect(s: String) = tok(s) // ! "expected '" + s + "'"
  def parens[A](p: Parser[String, A]) = tok("(") ~ p ~ expect(")")
  
  val expr: Parser[String, Expr] = mixfix(normal_app, Id, Applys, Operators)

  // grammar
  val id = Id.from(nonmixfix)
  val anyid = Id.from(name)

  val ids_dot = id.+ ~ "."
  val binder = Binds.from(bindfix_op, ids_dot, expr)
  val lambda = "lambda" ~ Lambdas.from(ids_dot, expr)

  val if_ = "if" ~ expr
  val then_ = "then" ~ expr
  val else_ = "else" ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ id
  val eq_ = "=" ~ expr
  val in_ = "in" ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val closed = parens(expr | anyid) | lambda | binder | ite | let | id
  val normal_app = Applys.from(closed, closed.*)

  val expr_eol = expr ~ ";"
  val exprs = expr_eol.*
  val command = Cmd.from(keyword, exprs)
  val commands = command.*

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


