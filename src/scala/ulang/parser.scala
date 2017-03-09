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
  val ebnf = Set(";", "*", "+", "(", ")", "{", "}", "[", "]", "|", "=", "end")

  val keywords = Set(",", ";", "(", ")", "{", "}", "[", "]", "->", "$", "`", "|", "\\",
    "if", "then", "else", "let", "in", "match", "with", "raise", "try", "catch", "end")

  val str = string collect {
    case s if s.head == '"' && s.last == '"' =>
      s.substring(1, s.length - 1)
  }

  val chr = string collect {
    case s if s.length == 3 && s.head == '\'' && s.last == '\'' =>
      s(1)
  }

  val name = string filterNot keywords
  val names = name.*
  val nonmixfix = name filterNot operators.contains

  val atom = Atom.from(nonmixfix)
  val anyatom = Atom.from(name)

  val left = lit("left", Left)
  val right = lit("right", Right)
  val assoc = left | right | ret(Non)

  val prefix = "prefix" ~! Prefix.from(int)
  val postfix = "postfix" ~! Postfix.from(int)
  val infix = "infix" ~! Infix.from(assoc, int)

  val fixity = prefix | postfix | infix

  val fix = Fix.from(fixity, names)
  val data = "data" ~! Data.from(names)

  val pat: Mixfix[List[String], Atom, Pat] = mixfix(inner_pat, Atom, UnApp, operators)
  val pats = pat.rep(sep = ",")

  val patarg: Parser[List[String], Pat] = P(("(" ~! patopen ~! ")") | force | any | patlist | patatom)
  val patargs = patarg.+

  val patnamed = "@" ~! patarg
  val patatom = nonmixfix ~ patnamed.? map {
    case ("_", None) => Wildcard
    case (name, None) => Atom(name)
    case ("_", Some(pat)) => pat
    case (name, Some(pat)) => SubPat(name, pat)
  }

  val expr: Mixfix[List[String], Atom, Expr] = mixfix(inner_expr, Atom, App, operators)
  val exprs = expr.rep(sep = ",")

  val arg: Parser[List[String], Expr] = P(("(" ~! open ~! ")") | fun | matches | raise | catches | ite | let | susp | escape | any | list | atom)
  val args = arg.+

  val if_ = "if" ~! expr
  val then_ = !"then" ~! expr
  val else_ = !"else" ~! expr
  val ite = IfThenElse.from(if_, then_, else_)
  val cond = if_.?

  val eq_ = LetEq.from(!patarg, !"=" ~! expr)
  val eqs_ = eq_.rep(sep = ",")
  val lets_ = "let" ~ eqs_
  val in_ = !"in" ~! expr
  val let = LetIn.from(lets_, in_)

  val arrow_ = !"->" ~! expr
  val cs = Case.from(pats, cond, arrow_)
  val cases = "|".? ~ (!cs).rep(sep = "|")

  val fun = "\\" ~! Bind.from(cases)

  val match_ = "match" ~! exprs
  val with_ = !"with" ~! cases
  val matches = MatchWith.from(match_, with_)

  val raise = "raise" ~! Raise.from(exprs)
  val try_ = "try" ~! expr
  val catch_ = !"catch" ~! cases
  val catches = TryCatch.from(try_, catch_)

  val force = "$" ~! Force.from(pat)
  val susp = "$" ~! Susp.from(expr)

  val any = Lit.from(str | chr)

  val tuple = exprs map builtin.reify_tuple
  val pattuple = pats map builtin.reify_tuple

  val open = tuple | anyatom
  val patopen = pattuple | anyatom

  val unapp = P(UnApp.from(patarg, patargs))
  val app = P(App.from(arg, args))

  val inner_pat = unapp | patarg
  val inner_expr = app | arg

  val escape = "`" ~! expr map builtin.reify

  val list = ("[" ~! exprs ~! "]") map builtin.reify_list
  val patlist = ("[" ~! pats ~! "]") map builtin.reify_list

  val expr_high = expr above 7
  val cond_high = ("if" ~! expr_high).?
  val pat_high = pat above 7
  val pat_low = pat above 2

  val eqq_ = !"=" ~! expr
  val df_eq = Def.from(pat_high, cond_high, "=" ~! expr)
  val df_eqv = Def.from(pat_low, cond, "<=>" ~! expr)

  val test = Test.from(expr)

  val rule: Parser[List[String], Rule] = P(alt)

  val nonebnf = string filterNot ebnf
  val id = Id.from(nonebnf)
  val tok = Tok.from(str)
  val ruleatom = ("(" ~! rule ~! ")") | tok | id

  val rep = ruleatom ~ (lit("*", false) | lit("+", true)).? map {
    case rule ~ None => rule
    case rule ~ Some(plus) => Rep(rule, plus)
  }

  val attr = "{" ~! expr ~! "}"
  val seq = Seq.from(rep.+, attr.?) | id
  val alt = Alt.from(seq.rep(sep = "|"))
  val prod = Prod.from(id ~! "=", !rule)

  def section[A, B](s0: String, c: List[A] => B, p: Parser[List[String], A], s1: String) = {
    val q = p ~! ";"
    s0 ~! c.from(q *) ~! s1
  }

  def named_section[A, B](s0: String, c: (String, List[A]) => B, n: Parser[List[String], String], p: Parser[List[String], A], s1: String) = {
    val q = p ~! ";"
    s0 ~! c.from(n, q *) ~! s1
  }

  val imports = "import" ~! Imports.from(names) ~! ";"
  val langs = "language" ~! Langs.from(names) ~! ";"
  val defs = section("define", Defs, df_eq | df_eqv, "end")
  val tests = section("test", Tests, test, "end")
  val nots = section("notation", Nots, fix | data, "end")
  val evals = section("eval", Evals, expr, "end")
  val grammar = section("grammar", Grammar, prod, "end")

  val cmd = imports | nots | defs | tests | evals | grammar | langs;
  val cmds = cmd *

  val module = Module.from(cmds)
}


