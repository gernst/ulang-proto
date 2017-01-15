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

  val keywords = Set(";", "(", ")", "{", "}", "[", "]", "->", "==", "$", "`", "|", "\\",
    "if", "then", "else", "let", "in", "match", "with", "end")

  val strict_int = expect("number", int)

  val str = string collect {
    case s if s.head == '"' && s.last == '"' =>
      s.substring(1, s.length - 1)
  }

  val chr = string collect {
    case s if s.length == 3 && s.head == '\'' && s.last == '\'' =>
      s(1)
  }

  val name = string filterNot keywords
  val names = name *
  val nonmixfix = name filterNot operators.contains

  val atom = Atom.from(nonmixfix)
  val anyatom = Atom.from(name)

  val left = lit("left", Left)
  val right = lit("right", Right)
  val assoc = left | right | ret(Non)

  val prefix = "prefix" ~ Prefix.from(strict_int)
  val postfix = "postfix" ~ Postfix.from(strict_int)
  val infix = "infix" ~ Infix.from(assoc, strict_int)

  val fixity = prefix | postfix | infix

  val fix = Fix.from(fixity, names)
  val data = "data" ~ Data.from(names)

  val pat: Parser[List[String], Pat] = mixfix(inner_pat, Atom, UnApp, operators)
  val pats = pat +
  val strict_pat = expect("pattern", pat)

  val patarg: Parser[List[String], Pat] = Parser.rec(parens("(", patopen, ")") | force |  any | patlist | patatom)
  val patargs = patarg +
  val strict_patarg = expect("closed pattern", patarg)
  val strict_patargs = expect("list of patterns", patargs)
  
  val patnamed = "@" ~ strict_patarg
  val patatom = nonmixfix ~ patnamed.? map {
    case (name, None) => Atom(name)
    case (name, Some(pat)) => SubPat(name, pat)
  }

  val expr: Parser[List[String], Expr] = mixfix(inner_expr, Atom, App, operators)
  val strict_expr = expect("expression", expr)

  val arg: Parser[List[String], Expr] = Parser.rec(parens("(", open, ")") | fun | matches | ite | let | susp | escape | any | list | atom)
  val args = arg +
  val strict_arg = expect("closed expression", arg)
  val strict_args = expect("list of expressions", args)

  val if_ = "if" ~ strict_expr
  val then_ = expect("then") ~ strict_expr
  val else_ = expect("else") ~ strict_expr
  val ite = IfThenElse.from(if_, then_, else_)
  val cond = if_ ?

  val let_ = "let" ~ strict_patarg
  val eq_ = expect("=") ~ strict_expr
  val in_ = expect("in") ~ strict_expr
  val let = LetIn.from(let_, eq_, in_)

  val arrow_ = "->" ~ strict_expr
  val cs = Case.from(patargs, cond, arrow_)
  val cases = "|".? ~ cs.rep(sep = "|")

  val fun = "\\" ~ Bind.from(cases)

  val match_ = "match" ~ strict_args
  val with_ = expect("with") ~ cases
  val matches = MatchWith.from(match_, with_)

  val force = "$" ~ Force.from(strict_pat)
  val susp = "$" ~ Susp.from(strict_expr)

  val any = Lit.from(str | chr)

  val open = expr | anyatom
  val patopen = pat | anyatom

  val unapp = UnApp.from(patarg, patargs)
  val app = App.from(arg, args)

  val inner_pat = unapp | patarg
  val inner_expr = app | arg

  val escape = "`" ~ strict_expr map builtin.reify
  val list = parens("[", arg *, "]") map builtin.reify_list
  val patlist = parens("[", patarg *, "]") map builtin.reify_list

  def section[A, B](s0: String, c: List[A] => B, p: Parser[List[String], A], s1: String) = {
    val q = p ~ expect(";")
    parens(s0, c.from(q *), s1)
  }

  val eqq_ = expect("==") ~ strict_expr
  val df = Def.from(pat, ret(None), eqq_)
  val df_cond = Def.from(pat, cond, eqq_)

  val test = Test.from(expr, eqq_)

  val imports = parens("import", Imports.from(names), ";")
  val patdefs = section("pattern", Pats, df, "end")
  val defs = section("define", Defs, df_cond, "end")
  val tests = section("test", Tests, test, "end")
  val nots = section("notation", Nots, fix | data, "end")
  val evals = section("eval", Evals, expr, "end")

  val cmd = imports | nots | patdefs | defs | tests | evals;
  val strict_cmd = expect("top level section", cmd)
  val cmds = strict_cmd *

  val module = Module.from(cmds)
}


