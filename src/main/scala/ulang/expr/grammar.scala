package ulang.expr

import java.io._

import scala.language.postfixOps

import arse._
import arse.implicits._
import sourcecode.Name

object operators extends Syntax[Id] {
  def is_mixfix_op(id: Id): Boolean = {
    contains(id) || is_bind_op(id)
  }
  
  def is_bind_op(id: Id) = {
    bind_ops contains id
  }

  var data: Set[Id] = Set()

  var prefix_ops: Map[Id, Int] = Map()
  var postfix_ops: Map[Id, Int] = Map()

  var infix_ops: Map[Id, (Assoc, Int)] = Map(
    builtin.eq.op -> (Non, 6))

  var bind_ops: Set[Id] = Set()
}

object scanner {
  // val string = scan("""\' ([^\']|\\\')* \' | \" ([^\"]|\\\")* \"""")
}

object grammar {
  val keywords = Set(",", ";", "(", ")", "{", "}", "[", "]", "->", "`", "|", "\\", "_",
    "as", "if", "then", "else", "let", "in", "match", "with", "end")

  val name = S("""[^ \r\n\t\f()\[\],;\'\"`]+""") filterNot keywords
  val names = name.*
  val wildcard = Wildcard("_")

  val anyatom = Id(name)
  val atom = anyatom filterNot operators.is_mixfix_op
  val bindatom = anyatom filter operators.is_bind_op

  val pat: Mixfix[Id, Pat] = M(inner_pat, anyatom, UnApps, operators)
  val pats = pat ~+ ","

  val patarg: Parser[Pat] = P(("(" ~ patopen ~ ")") | patlist | wildcard | atom) ~ ("as" ~ atom).? map {
    case pat ~ None => pat
    case pat ~ Some(x: Var) => Named(pat, x)
    case _ => ???
  }

  val patargs = patarg.+

  val expr: Mixfix[Id, Expr] = M(inner_expr, anyatom, Apps, operators)
  val exprs = expr ~+ ","

  val arg: Parser[Expr] = P(("(" ~ open ~ ")") | lambda | binder | ite | /* matches | */ let | quote | any | list | atom)
  val args = arg +

  val abs = patargs ~ "->" ~ expr
  val lambda = Lambda("\\" ~ abs)
  val binder = App(bindatom ~ Lambda(abs))

  val ite = ("if" ~ expr ~ "then" ~ expr ~ "else" ~ expr) map {
    case test ~ left ~ right => builtin.IfThenElse(test, left, right)
  }

  // val matches = MatchWith("match" ~ args ~ "with" ~ bindings)

  val eq = patarg ~ "=" ~ expr // TODO: pat_high instead of patarg
  val eqs = eq ~+ ","
  val let = LetIn("let" ~ eqs ~ "in" ~ expr)

  val any = (string | char) map { Lit(_) }

  val tuple = exprs map reify.tuple
  val pattuple = pats map reify.tuple

  val open = tuple | anyatom | ret(builtin.Tuple(): Expr)
  val patopen = pattuple | anyatom | ret(builtin.Tuple(): Pat)

  val inner_pat = (patarg +) map {
    case Nil => ???
    case fun :: args => UnApps(fun, args)
  }

  val inner_expr = args map {
    case Nil => ???
    case fun :: args => Apps(fun, args)
  }

  val quote = "`" ~ expr map reify.reify

  val nil = ret(builtin.Nil)
  val tail = (";" ~ expr) | nil
  val cons = exprs ~ tail map { case (exprs, tail) => reify.list_with_tail(exprs, tail) }
  val list = "[" ~ (cons | nil) ~ "]"

  val pattail = (";" ~ pat) | nil
  val uncons = pats ~ pattail map { case (pats, tail) => reify.list_with_tail(pats, tail) }
  val patlist = "[" ~ (uncons | nil) ~ "]"
}


