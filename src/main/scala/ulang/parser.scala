package ulang

import java.io._

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

import arse._
import sourcecode.Name

object operators extends Syntax[String] {
  var data: Set[String] = Set(",")
  var prefix_ops: Map[String, Int] = Map()
  var postfix_ops: Map[String, Int] = Map()
  var infix_ops: Map[String, (Assoc, Int)] = Map("=" -> (Non, 6))
}

object parser {
  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(line: String) = new StringReader(line)

  def parse[A](p: Parser[A], reader: Reader): A = {
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
  val ebnf = Set(";", "*", "+", "(", ")", "{", "}", "[", "]", "|", "=", "end")

  val keywords = Set(",", ";", "(", ")", "{", "}", "[", "]", "->", "$", "`", "|", "\\", "_",
    "as", "if", "then", "else", "let", "in", "match", "with", "raise", "try", "catch", "end")

  val name = string filterNot keywords
  val names = name.*
  val nonmixfix = name filterNot operators.contains
  val wildcard = lit("_", Wildcard)

  val atom = nonmixfix ^^ { Atom }
  val anyatom = name ^^ { Atom }

  val left = lit("left", Left)
  val right = lit("right", Right)
  val assoc = left | right | ret(Non)

  val prefix = "prefix" ~ int ^^ { Prefix }
  val postfix = "postfix" ~ int ^^ { Postfix }
  val infix = "infix" ~ assoc ~ int ^^ { Infix }

  val fixity = prefix | postfix | infix

  val fix = fixity ~ names ^^ { Fix }
  val data = "data" ~ names ^^ { Data }

  val pat: Mixfix[Atom, Pat] = mixfix(inner_pat, Atom, UnApp, operators)
  val pats = pat ~* ","

  val patarg: Parser[Pat] = P(("(" ~ patopen ~ ")") | any | patlist | wildcard | atom) ~ ("as" ~ nonmixfix).? map {
    case pat ~ None => pat
    case pat ~ Some(name) => SubPat(name, pat)
  }

  val patargs = patarg.+

  val expr: Mixfix[Atom, Expr] = mixfix(inner_expr, Atom, App, operators)
  val exprs = expr ~* ","

  val arg: Parser[Expr] = P(("(" ~ open ~ ")") | bind | matches | ite | let | escape | any | list | atom)
  val args = arg +

  val eq = patarg ~ "=" ~ expr ^^ { LetEq }
  val eqs = eq ~* ","
  val let = "let" ~ eqs ~ "in" ~ expr ^^ { LetIn }

  val cond = "if" ~ expr ?
  val cs = pats ~ cond ~ "->" ~ expr ^^ { Case }
  val cases = ("|" ?) ~ (cs ~* "|")

  val bind = "\\" ~ cases ^^ { Bind }
  val ite = "if" ~ expr ~ "then" ~ expr ~ "else" ~ expr ^^ { IfThenElse }
  val matches = "match" ~ exprs ~ "with" ~ cases ^^ { MatchWith }

  val any = (string | char) ^^ { Lit }

  val tuple = exprs ^^ builtin.reify_tuple
  val pattuple = pats ^^ builtin.reify_tuple

  val open = tuple | anyatom | ret(builtin.Unit)
  val patopen = pattuple | anyatom | ret(builtin.UnUnit)

  val unapp = P(patarg ~ patargs) ^^ { UnApp }
  val app = P(arg ~ args) ^^ { App }

  val inner_pat = unapp | patarg
  val inner_expr = app | arg

  val escape = "`" ~ expr ^^ builtin.reify

  val nil = "[" ~ ret(builtin.Nil) ~ "]"

  val tail = (";" ~ expr) | ret(builtin.Nil)
  val cons = "[" ~ exprs ~ tail ~ "]" ^^ builtin.reify_list_with_tail
  val list = cons | nil

  val pattail = (";" ~ pat) | ret(builtin.Nil)
  val uncons = "[" ~ pats ~ pattail ~ "]" ^^ builtin.reify_list_with_tail
  val patlist = uncons | nil

  val expr_high = expr above 7
  val cond_high = "if" ~ expr_high ?
  val pat_high = pat above 7
  val pat_low = pat above 2

  val df_eq = pat_high ~ cond_high ~ "=" ~ expr ^^ { Def }
  val df_eqv = pat_low ~ cond ~ "<=>" ~ expr ^^ { Def }

  val test = expr ^^ { Test }

  val rule: Parser[Rule] = P(alt)

  val nonebnf = string filterNot ebnf
  val id = nonebnf ^^ { Id }
  val tok = str ^^ { Tok }
  val ruleatom = ("(" ~ rule ~ ")") | tok | id

  val rep = ruleatom ~ (lit("*", false) | lit("+", true)).? map {
    case rule ~ None => rule
    case rule ~ Some(plus) => Rep(rule, plus)
  }

  val attr = "{" ~ expr ~ "}" ?
  val seq = (rep.* ~ attr) ^^ { Seq } | id
  val alt = seq ~* "|" ^^ { Alt }
  val prod = id ~ "=" ~ rule ^^ { Prod }

  def section[A, B](s0: String, c: List[A] => B, p: Parser[A], s1: String) = {
    val q = p ~ ";"
    s0 ~ q.* ~ s1 ^^ c
  }

  def named_section[A, B](s0: String, c: (String, List[A]) => B, n: Parser[String], p: Parser[A], s1: String) = {
    val q = p ~ ";"
    s0 ~ n ~ q.* ~ s1 ^^ c
  }

  val imports = "import" ~ names ~ ";" ^^ { Imports }
  val langs = "language" ~ names ~ ";" ^^ { Langs }
  val defs = section("define", Defs, df_eq | df_eqv, "end")
  val tests = section("test", Tests, test, "end")
  val nots = section("notation", Nots, fix | data, "end")
  val evals = section("eval", Evals, expr, "end")
  val grammar = section("grammar", Grammar, prod, "end")

  val cmd = imports | nots | defs | tests | evals | grammar | langs;

  val module = cmd.* ^^ { Module }
}


