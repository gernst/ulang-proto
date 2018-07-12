package ulang.expr

class Unary(val op: Id) {
  def unapply(p: Pat) = p match {
    case UnApp(`op`, List(arg)) => Some(arg)
    case _ => None
  }

  def unapply(e: Expr) = e match {
    case App(`op`, List(arg)) => Some(arg)
    case _ => None
  }

  def unapply(v: Val) = v match {
    case Obj(`op`, List(arg)) => Some(arg)
    case _ => None
  }

  def apply(arg: Expr) = {
    App(op, List(arg))
  }

  def apply(arg: Pat) = {
    UnApp(op, List(arg))
  }
}

class Binary(val op: Id) {
  def unapply(p: Pat) = p match {
    case UnApp(`op`, List(arg1, arg2)) => Some((arg1, arg2))
    case _ => None
  }

  def unapply(e: Expr) = e match {
    case App(`op`, List(arg1, arg2)) => Some((arg1, arg2))
    case _ => None
  }

  def unapply(v: Val) = v match {
    case Obj(`op`, List(arg1, arg2)) => Some((arg1, arg2))
    case _ => None
  }

  def apply(arg1: Expr, arg2: Expr): Expr = {
    App(op, List(arg1, arg2))
  }

  def apply(args: List[Expr], zero: Expr): Expr = {
    args.foldRight(zero)(apply)
  }

  def apply(zero: Expr, args: List[Expr]): Expr = {
    args.foldLeft(zero)(apply)
  }

  def apply(arg1: Pat, arg2: Pat): Pat = {
    UnApp(op, List(arg1, arg2))
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
    case UnApp(`op`, args) => Some(args)
    case _ => None
  }

  def unapplySeq(e: Expr) = e match {
    case App(`op`, args) => Some(args)
    case _ => None
  }

  def unapplySeq(v: Val) = v match {
    case Obj(`op`, args) => Some(args)
    case _ => None
  }

  def apply(args: Expr*) = {
    App(op, args.toList)
  }

  def apply(args: List[Expr]) = {
    App(op, args)
  }

  def apply(args: Pat*) = {
    UnApp(op, args.toList)
  }

  def apply(args: List[Pat]) = {
    UnApp(op, args)
  }
}