package ulang.core

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
