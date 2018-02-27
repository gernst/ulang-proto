package ulang

import java.io._

import scala.language.postfixOps

import arse._
import arse.implicits._
import sourcecode.Name

object operators extends Syntax[Atom] {
  def contains(name: String): Boolean = contains(Atom(name))

  var data: Set[Atom] = Set()

  var prefix_ops: Map[Atom, Int] = Map()
  var postfix_ops: Map[Atom, Int] = Map()

  var infix_ops: Map[Atom, (Assoc, Int)] = Map(
    builtin.Eq -> (Non, 6))
}

object scanner {
  // val string = scan("""\' ([^\']|\\\')* \' | \" ([^\"]|\\\")* \"""")
}

object grammar {
  val keywords = Set(",", ";", "(", ")", "{", "}", "[", "]", "->", "`", "|", "\\", "_",
    "as", "if", "then", "else", "let", "in", "match", "with", "end")

  val name = S("""[^ \r\n\t\f()\[\],;\'\"`]+""") filterNot keywords
  val names = name.*
  val nonmixfix = name filterNot operators.contains
  val wildcard = Wildcard("_")

  val atom = Atom(nonmixfix)
  val anyatom = Atom(name)

  val left = Left("left")
  val right = Right("right")
  val assoc = left | right | ret(Non)

  val prefix = Prefix("prefix" ~ int)
  val postfix = Postfix("postfix" ~ int)
  val infix = Infix("infix" ~ assoc ~ int)

  val fixity = prefix | postfix | infix

  val fix = Fix(fixity ~ names)
  val data = Data("data" ~ names)

  val pat: Mixfix[Atom, Pat] = M(inner_pat, anyatom, UnApp, operators)
  val pats = pat ~+ ","

  val patarg: Parser[Pat] = P(("(" ~ patopen ~ ")") | any | patlist | wildcard | atom) ~ ("as" ~ nonmixfix).? map {
    case pat ~ None => pat
    case pat ~ Some(name) => SubPat(name, pat)
  }

  val patargs = patarg.+

  val expr: Mixfix[Atom, Expr] = M(inner_expr, anyatom, App, operators)
  val exprs = expr ~+ ","

  val arg: Parser[Expr] = P(("(" ~ open ~ ")") | bind | matches | ite | let | escape | any | list | atom)
  val args = arg +

  val eq = LetEq(patarg ~ "=" ~ expr)
  val eqs = eq ~+ ","
  val let = LetIn("let" ~ eqs ~ "in" ~ expr)

  val cond = "if" ~ expr
  val cs = Case(pats ~ (cond ?) ~ "->" ~ expr)
  val cases = cs ~+ "|"

  val bind = Bind("\\" ~ cases)
  val ite = IfThenElse("if" ~ expr ~ "then" ~ expr ~ "else" ~ expr)
  val matches = MatchWith("match" ~ exprs ~ "with" ~ cases)

  val any = (string | char) map { Lit(_) }

  val tuple = exprs map builtin.reify_tuple
  val pattuple = pats map builtin.reify_tuple

  val open = tuple | anyatom | ret(builtin.Unit)
  val patopen = pattuple | anyatom | ret(builtin.UnUnit)

  val inner_pat = (patarg +) map {
    case Nil => ???
    case const :: Nil => const
    case fun :: args => UnApp(fun, args)
  }

  val inner_expr = (arg +) map {
    case Nil => ???
    case const :: Nil => const
    case fun :: args => App(fun, args)
  }

  val escape = "`" ~ expr map builtin.reify

  val nil = ret(builtin.Nil)
  val tail = (";" ~ expr) | nil
  val cons = exprs ~ tail map { case (exprs, tail) => builtin.reify_list_with_tail(exprs, tail) }
  val list = "[" ~ (cons | nil) ~ "]"

  val pattail = (";" ~ pat) | nil
  val uncons = pats ~ pattail map { case (pats, tail) => builtin.reify_list_with_tail(pats, tail) }
  val patlist = "[" ~ (uncons | nil) ~ "]"

  val expr_high = expr above 7
  val cond_high = "if" ~ expr_high
  val pat_high = pat above 7

  val df = Def(pat_high ~ (cond_high ?) ~ "=" ~ expr)

  val test = Test(expr)

  val def_ = (df ~ ";") *
  val test_ = (test ~ ";") *
  val notation_ = ((fix | data) ~ ";") *
  val expr_ = (expr ~ ";") *

  val imports = Imports("import" ~ names ~ ";")
  val defs = Defs("define" ~ def_ ~ "end")
  val tests = Tests("test" ~ test_ ~ "end")
  val notations = Notations("notation" ~ notation_ ~ "end")
  val evals = Evals("eval" ~ expr_ ~ "end")

  val cmd = imports | notations | defs | tests | evals;

  val module = Module(cmd *)
}


