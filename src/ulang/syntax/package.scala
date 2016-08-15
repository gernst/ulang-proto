package ulang

import ulang._

package object syntax {
  type Subst = Map[String, Expr]
  
  object Subst {
    val empty: Subst = Map.empty
  }
  
  implicit class IsConstr(str: String) {
    def isConstr = str.head.isUpper
  }
  
  val Wildcard = Id("_")
  val True = Const("True")
  val False = Const("False")

  object LetIn extends Ternary("let_=_in_")
  object IfThenElse extends Ternary("if_then_else_")

  object Eq extends Binary("==")

  object Not extends Unary("not")
  object And extends Binary("and")
  object Or extends Binary("or")
  object Imp extends Binary("==>")
  object Eqv extends Binary("<==>")

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
    case Id(name) =>
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