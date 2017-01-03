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

  val keywords = Set(";", "(", ")", "{", "}", "[", "]", "->", "==", "|", "\\",
    "if", "then", "else", "let", "in", "match", "with", "end")

  val name = string filterNot keywords
  val names = name *
  val nonmixfix = name filterNot operators.contains

  val expr: Parser[List[String], Expr] = mixfix(inner, Atom, App, operators)
  val exprs = expr +

  val closed: Parser[List[String], Expr] = Parser.rec(parens(open) | fun | matches | ite | let | id)
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

  val arrow_ = "->" ~ expr
  val cs = Case.from(exprs, arrow_)
  val cases = "|".? ~ cs.rep(sep = "|")

  val fun = "\\" ~ Bind.from(cases)

  val if_ = "if" ~ expr
  val then_ = expect("then") ~ expr
  val else_ = expect("else") ~ expr
  val ite = IfThenElse.from(if_, then_, else_)

  val let_ = "let" ~ closed
  val eq_ = expect("=") ~ expr
  val in_ = expect("in") ~ expr
  val let = LetIn.from(let_, eq_, in_)

  val match_ = "match" ~ closeds
  val with_ = expect("with") ~ cases
  val matches = Match.from(match_, with_)

  val open = expr | anyid

  val app = App.from(closed, closed +)
  val inner = app | closed

  val imports = "import" ~ Imports.from(names) ~ expect(";")

  val lhs = expr ~ expect("==")
  val rhs = expr ~ expect(";")
  val df = Def.from(lhs, rhs)
  val defs = "definitions" ~ Defs.from(df *) ~ "end"

  val not = (fix | data) ~ expect(";")
  val nots = "notation" ~ Nots.from(not *) ~ "end"

  val evals = "eval" ~ Evals.from(rhs *) ~ "end"

  val cmd = imports | nots | defs | evals;
  val cmds = cmd *

  val module = Module.from(cmds)
}


