package ulang.core

abstract class IdPred extends (String => Id) {
  def test(name: String): Boolean

  def apply(name: String) = {
    assert(test(name))
    Id(name)
  }

  def unapply(id: Id): Option[String] = id match {
    case Id(name) if test(name) =>
      Some(name)
    case _ =>
      None
  }
}

object Op extends IdPred {
  def test(str: String) = Operators contains str
}

object Tag extends IdPred {
  def test(str: String) = str.head.isUpper || Operators.constrs(str)
}

object Lambda extends ((List[Expr], Expr) => Expr) {
  def apply(pats: List[Expr], body: Expr) = {
    Bind(List(Case(pats, body)))
  }

  def unapply(expr: Expr): Option[(List[Expr], Expr)] = expr match {
    case Bind(List(Case(pats, body))) => Some((pats, body))
    case _ => None
  }
}

class Unary(name: String) extends (Expr => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(`op`, List(arg)) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Expr) = {
    Apply(op, List(arg))
  }
}

class Binary(name: String) extends ((Expr, Expr) => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(`op`, List(arg1, arg2)) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr) = {
    Apply(op, List(arg1, arg2))
  }
}
