package ulang.syntax

import arse._
import java.io.Reader
import java.io.File
import java.io.FileReader
import scala.collection.mutable.ListBuffer

import ulang._

object Parser extends Combinators with Mixfix {
  import Combinators._
  import Operators._

  type T = String

  type Op = ulang.Expr
  type Expr = ulang.Expr

  val keywords = Set("definitions", "inductive", "axioms", "lemmas", "theorems")
  val special = Set(";", ".", "(", ")", "lambda") ++ keywords
  val eol = lit(";")
  val dot = lit(".")

  def unary(op: Op, arg: Expr): Expr = Apply(op, arg)
  def binary(op: Op, arg1: Expr, arg2: Expr): Expr = Apply(Apply(op, arg1), arg2)

  def mixfix_op[A](m: Map[String, A], name: String) = {
    if (keywords contains name) fail
    else if (m contains name) (Id(name), m(name))
    else fail
  }

  def mixfix_op(s: Set[String], name: String) = {
    if (keywords contains name) fail
    else if (s contains name) Id(name)
    else fail
  }

  val name = __ filterNot special
  val keyword = __ filter keywords
  val nonmixfix = name filterNot Operators.contains
  def expect(s: String) = lit(s) ! "expected '" + s + "'"
  def parens[A](p: Parser[T, A]) = lit("(") ~> p <~ expect(")")

  val prefix_op = next { mixfix_op(prefix_ops, _) }
  val postfix_op = next { mixfix_op(postfix_ops, _) }
  val infix_op = next { mixfix_op(infix_ops, _) }
  val bindfix_op = next { mixfix_op(bindfix_ops, _) }

  // grammar
  val id = parse(Id)(nonmixfix)
  val anyop = parse(Id)(name)

  val ids_dot = id.+ <~ dot
  val binder = parse(Binds)(bindfix_op, ids_dot, expr)
  val lambda = lit("lambda") ~> parse(Lambdas)(ids_dot, expr)
  val closed = parens(expr | anyop) | lambda | binder | id
  val normal_app = parse(Applys)(closed, closed.*)

  val inner_expr: Parser[String, Expr] = normal_app

  def expr = mixfix_expr
  val expr_eol = expr <~ eol
  val exprs = expr_eol *
  val command = parse(Cmd)(keyword, exprs)
  val commands = command *

  def load(file: File) = {
    val in = tokenize(new FileReader(file))
    val p = commands.$
    p(in)
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


