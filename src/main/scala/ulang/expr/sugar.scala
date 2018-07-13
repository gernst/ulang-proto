package ulang.expr

object UnApps extends ((Pat, List[Pat]) => Pat) {
  def apply(fun: Pat, args: List[Pat]): Pat = {
    args.foldLeft(fun)(UnApp)
  }

  def flatten(expr: Pat, args: List[Pat]): (Pat, List[Pat]) = expr match {
    case UnApp(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }

  def unapply(expr: Pat): Option[(Pat, List[Pat])] = {
    Some(flatten(expr, Nil))
  }
}

object Apps extends ((Expr, List[Expr]) => Expr) {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(App)
  }

  def flatten(expr: Expr, args: List[Expr]): (Expr, List[Expr]) = expr match {
    case App(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }

  def unapply(expr: Expr): Option[(Expr, List[Expr])] = {
    Some(flatten(expr, Nil))
  }
}

class Unary(val op: Id) {
  def unapply(p: Pat) = p match {
    case UnApp(`op`, arg) => Some(arg)
    case _ => None
  }

  def unapply(e: Expr) = e match {
    case App(`op`, arg) => Some(arg)
    case _ => None
  }

  def unapply(v: Val) = v match {
    case Obj(`op`, arg) => Some(arg)
    case _ => None
  }

  def apply(arg: Expr) = {
    App(op, arg)
  }

  def apply(arg: Pat) = {
    UnApp(op, arg)
  }
}

class Binary(val op: Id) {
  def unapply(p: Pat) = p match {
    case UnApp(UnApp(`op`, arg1), arg2) => Some((arg1, arg2))
    case _ => None
  }

  def unapply(e: Expr) = e match {
    case App(App(`op`, arg1), arg2) => Some((arg1, arg2))
    case _ => None
  }

  def unapply(v: Val) = v match {
    case Objs(`op`, List(arg1, arg2)) => Some((arg1, arg2))
    case _ => None
  }

  def apply(arg1: Expr, arg2: Expr): Expr = {
    App(App(op, arg1), arg2)
  }

  def apply(args: List[Expr], zero: Expr): Expr = {
    args.foldRight(zero)(apply)
  }

  def apply(zero: Expr, args: List[Expr]): Expr = {
    args.foldLeft(zero)(apply)
  }

  def apply(arg1: Pat, arg2: Pat): Pat = {
    UnApp(UnApp(op, arg1), arg2)
  }

  def apply(args: List[Pat], zero: Pat): Pat = {
    args.foldRight(zero)(apply)
  }

  def apply(zero: Pat, args: List[Pat]): Pat = {
    args.foldLeft(zero)(apply)
  }
}

class Nary(val op: Id) {
  def unapplySeq(p: Pat) = p match {
    case UnApps(`op`, args) => Some(args)
    case _ => None
  }

  def unapplySeq(e: Expr) = e match {
    case Apps(`op`, args) => Some(args)
    case _ => None
  }

  def unapplySeq(v: Val) = v match {
    case Objs(`op`, args) => Some(args)
    case _ => None
  }

  def apply(args: Expr*) = {
    Apps(op, args.toList)
  }

  def apply(args: List[Expr]) = {
    Apps(op, args)
  }

  def apply(args: Pat*) = {
    UnApps(op, args.toList)
  }

  def apply(args: List[Pat]) = {
    UnApps(op, args)
  }
}