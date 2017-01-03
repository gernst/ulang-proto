package ulang

import java.io._

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

import arse._

object operators extends Syntax[String] {
  var data: Set[String] = Set()
  var prefix_ops: Map[String, Int] = Map()
  var postfix_ops: Map[String, Int] = Map()
  var infix_ops: Map[String, (Assoc, Int)] = Map()
}

object parser {
  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(line: String) = new StringReader(line)

  def parse[A](p: Parser[List[String], A], reader: Reader): A = {
    val (res, out) = p(tokenize(reader))
    if (!out.isEmpty)
      sys.error("remaining input: " + out.mkString(" "))
    res
  }

  def tokenize(reader: Reader): List[String] = {
    val scanner = new scanner(reader)
    val res = new ListBuffer[String]()
    var tok = scanner.next()
    while (tok != null) {
      res += tok
      tok = scanner.next()
    }
    res.toList
  }
}

object grammar {
  import arse.Parser._
  import arse.Recognizer._
  import arse.Mixfix._

  val keywords = Set(";", "(", ")", "{", "}", "[", "]", "->", "==", "$", "|", "\\",
    "if", "then", "else", "let", "in", "match", "with", "end")

  val strict_int = expect("number", int)
  val name = string filterNot keywords
  val names = name *
  val nonmixfix = name filterNot operators.contains

  val expr: Parser[List[String], Expr] = mixfix(inner, Atom, App, operators)
  val strict_expr = expect("expression", expr)

  val arg: Parser[List[String], Expr] = Parser.rec(parens("(", open, ")") | fun | matches | ite | let | susp | list | id)
  // val pats = expect("patterns", arg +)
  val args = arg +
  val strict_arg = expect("closed expression", arg)
  val strict_args = expect("list of expressions", args)

  val left = lit("left", Left)
  val right = lit("right", Right)
  val assoc = left | right | ret(Non)

  val prefix = "prefix" ~ Prefix.from(strict_int)
  val postfix = "postfix" ~ Postfix.from(strict_int)
  val infix = "infix" ~ Infix.from(assoc, strict_int)

  val fixity = prefix | postfix | infix

  val fix = Fix.from(fixity, names)
  val data = "data" ~ Data.from(names)

  val id = Atom.from(nonmixfix)
  val anyid = Atom.from(name)

  val if_ = "if" ~ strict_expr
  val then_ = expect("then") ~ strict_expr
  val else_ = expect("else") ~ strict_expr
  val ite = IfThenElse.from(if_, then_, else_)
  val cond = if_ ?

  val let_ = "let" ~ strict_arg
  val eq_ = expect("=") ~ strict_expr
  val in_ = expect("in") ~ strict_expr
  val let = LetIn.from(let_, eq_, in_)

  val arrow_ = "->" ~ strict_expr
  val cs = Case.from(args, cond, arrow_)
  val cases = "|".? ~ cs.rep(sep = "|")

  val fun = "\\" ~ Bind.from(cases)

  val match_ = "match" ~ strict_args
  val with_ = expect("with") ~ cases
  val matches = Match.from(match_, with_)

  val susp = "$" ~ Susp.from(strict_expr)

  val open = expr | anyid

  val app = App.from(arg, args)
  val inner = app | arg

  val list = parens("[", arg *, "]") map builtin.reify

  def section[A, B](s0: String, c: List[A] => B, p: Parser[List[String], A], s1: String) = {
    val q = p ~ expect(";")
    parens(s0, c.from(q *), s1)
  }

  val eqq_ = expect("==") ~ strict_expr
  val df = Def.from(expr, ret(None), eqq_)
  val df_cond = Def.from(expr, cond, eqq_)

  val imports = parens("import", Imports.from(names), ";")
  val pats = section("pattern", Pats, df, "end")
  val defs = section("define", Defs, df_cond, "end")
  val tests = section("test", Tests, df, "end")
  val nots = section("notation", Nots, fix | data, "end")
  val evals = section("eval", Evals, expr, "end")

  val cmd = imports | nots | pats | defs | tests | evals;
  val cmds = cmd *

  val module = Module.from(cmds)
}


