package ulang

import ulang._

package object syntax {
  object LetIn extends Ternary(Id("let_=_in_"))
  object IfThenElse extends Ternary(Id("if_then_else_"))

  object Eq extends Binary(Id("=="))

  object Not extends Unary(Id("not"))
  object And extends Binary(Id("and"))
  object Or extends Binary(Id("or"))
  object Imp extends Binary(Id("==>"))
  object Eqv extends Binary(Id("<==>"))

  val Zero = Id("Zero")
  object Succ extends Unary(Id("Succ"))
  object UnaryMinus extends Unary(Id("-"))
  object Plus extends Binary(Id("+"))
  object Minus extends Binary(Id("-"))
  object Mult extends Binary(Id("*"))

  implicit val ev1 = ExprOrdering

  object Def {
    def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
      case Eq(lhs, rhs) =>
        Some((lhs, rhs))
      case Eqv(lhs, rhs) =>
        Some((lhs, rhs))
      case _ =>
        None
    }
  }

  def free(expr: Expr): Set[String] = expr match {
    case Var(name) =>
      Set(name)
    case Apply(fun, arg) =>
      free(fun) ++ free(arg)
    case Match(cases) =>
      cases.foldLeft(Set.empty[String]) {
        case (s, Case(bound, body)) =>
          s ++ (free(body) -- free(bound))
      }
    case _ =>
      Set()
  }
}