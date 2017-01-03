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

  val name = string filterNot keywords
  val names = name *
  val nonmixfix = name filterNot operators.contains

  val expr: Parser[List[String], Expr] = mixfix(inner, Atom, App, operators)
  val exprs = expr +

  val strict_expr = expect("expression", expr)

  val closed: Parser[List[String], Expr] = Parser.rec(parens("(", open, ")") | fun | matches | ite | let | lzy | list | id)
  val closeds = closed +

  val left = lit("left", Left)
  val right = lit("right", Right)
  val non = ret[List[String], Assoc](Non)

  val assoc = left | right | non

  val prefix = "prefix" ~ Prefix.from(int)
  val postfix = "postfix" ~ Postfix.from(int)
  val infix = "infix" ~ Infix.from(assoc, int)

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

  val let_ = "let" ~ closed
  val eq_ = expect("=") ~ strict_expr
  val in_ = expect("in") ~ strict_expr
  val let = LetIn.from(let_, eq_, in_)

  val arrow_ = "->" ~ strict_expr
  val cs = Case.from(exprs, cond, arrow_) // why not closeds
  val cases = "|".? ~ cs.rep(sep = "|")

  val fun = "\\" ~ Bind.from(cases)

  val match_ = "match" ~ closeds
  val with_ = expect("with") ~ cases
  val matches = Match.from(match_, with_)

  val lzy = "$" ~ Lazy.from(expr)

  val open = expr | anyid

  val app = App.from(closed, closeds)
  val inner = app | closed

  val list = parens("[", closeds, "]") map builtin.reify

  val imports = "import" ~ Imports.from(names) ~ expect(";")

  val rhs = expect("==") ~ strict_expr ~ expect(";")

  val df = Def.from(expr, ret(None), rhs)
  val dfs = df *

  val df_cond = Def.from(expr, cond, rhs)
  val dfs_cond = df_cond *

  val pats = "pattern" ~ Pats.from(dfs) ~ "end"
  val defs = "define" ~ Defs.from(dfs_cond) ~ "end"
  val tests = "test" ~ Tests.from(dfs) ~ "end"

  val not = (fix | data) ~ expect(";")
  val nots = "notation" ~ Nots.from(not *) ~ "end"

  val eval = expr ~ expect(";")
  val evals = "eval" ~ Evals.from(eval *) ~ "end"

  val cmd = imports | nots | pats | defs | tests | evals;
  val cmds = cmd *

  val module = Module.from(cmds)
}


