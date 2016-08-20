package ulang.core

import scala.language.implicitConversions
import scala.language.postfixOps

import java.io.Reader
import java.io.File
import java.io.FileReader

import scala.collection.mutable.ListBuffer

import arse._

import ulang._

object Parser {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  val keywords = Set("definitions", "inductive", "axioms", "lemmas", "theorems")
  val special = Set(";", ".", "(", ")", "=", "lambda", "if", "then", "else", "let", "in")

  val name = string filterNot special
  val tag = string filter isTag
  val nonmixfix = name filter isFun

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")

  val pat: Parser[List[String], Pat] = mixfix(constr, identity, Constr, Constrs)
  val pats = parens(pat) *

  val expr: Parser[List[String], Expr] = mixfix(app, Id, Apply, Funs)
  val exprs = expr +

  // grammar
  val id = Id.from(nonmixfix)
  val anyid = Id.from(name)

  val dot_ = "." ~ expr
  val cs = Case.from(pats, dot_)
  val lambda = "lambda" ~ Lambda.from(cs *)

  val if_ = "if" ~ expr
  val then_ = "then" ~ expr
  val else_ = "else" ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ pat
  val eq_ = "=" ~ expr
  val in_ = "in" ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val constr = Constr.from(tag, pats)

  val closed = parens(expr | anyid) | lambda | ite | let | id
  val app = Apply.from(closed, closed *)

  val expr_eol = expr ~ ";"

  /*
  def load(file: File): List[Cmd] = {
    val in = tokenize(new FileReader(file))
    /* val (res, Nil) = commands(in)
    res */
    ???
  }
  */

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


