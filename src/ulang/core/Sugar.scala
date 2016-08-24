package ulang.core

object Op {
  def unapply(id: Id): Option[String] = {
    if(Operators contains id.name)
      Some(id.name)
    else
      None
  }
}

object Tag extends (String => Id) {
  def apply(name: String) = {
    assert(isTag(name))
    Id(name)
  }
  
  def unapply(id: Id): Option[String] = id match {
    case Id(name) if isTag(name) =>
      Some(name)
    case _ =>
      None
  }
}

object Applys extends ((Expr, List[Expr]) => Expr) {
  def apply(fun: Expr, args: List[Expr]) = {
    args.foldLeft(fun)(Apply)
  }

  def unapply(expr: Expr) = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Expr, args: List[Expr]): (Expr, List[Expr]) = expr match {
    case Apply(fun, arg) => flatten(fun, arg :: args)
    case _ => (expr, args)
  }
}

object Lambda extends ((Expr, Expr) => Expr) {
  def apply(pat: Expr, body: Expr) = {
    Bind(List(Case(pat, body)))
  }
  
  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
    case Bind(List(Case(pat, body))) => Some((pat, body))
    case _ => None
  }
}

object Lambdas extends ((List[Expr], Expr) => Expr) {
  def apply(pats: List[Expr], body: Expr) = {
    pats.foldRight(body)(Lambda)
  }
  
  def unapply(expr: Expr) = {
    Some(flatten(expr))
  }
  
  def flatten(expr: Expr): (List[Expr], Expr) = expr match {
    case Lambda(pat, body) =>
      val (pats, inner) = flatten(body)
      (pat :: pats, inner)
    case _ => (Nil, expr)
  }
}

class Unary(name: String) extends (Expr => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(`op`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Expr) = {
    Apply(op, arg)
  }
}

class Binary(name: String) extends ((Expr, Expr) => Expr) {
  val op = Id(name)

  def unapply(e: Expr) = e match {
    case Apply(Apply(`op`, arg1), arg2) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr) = {
    Apply(Apply(op, arg1), arg2)
  }
}
