package ulang.expr

object LetIn extends ((List[(Pat, Expr)], Expr) => Expr) {
  def apply(eqs: List[(Pat, Expr)], body: Expr): Expr = {
    val (pats, args) = eqs.unzip
    Apps(Lambda(pats, body), args)
  }
}

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

object Objs extends ((Data, List[Val]) => Val) {
  def apply(fun: Data, args: List[Val]): Val = {
    args.foldLeft(fun)(Obj)
  }

  def flatten(any: Data, args: List[Val]): (Data, List[Val]) = any match {
    case Obj(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (any, args)
  }

  def unapply(any: Data): Option[(Data, List[Val])] = {
    Some(flatten(any, Nil))
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
    case Obj(Obj(`op`, arg1), arg2) => Some((arg1, arg2))
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

class Ternary(val op: Id) {
  def unapply(p: Pat) = p match {
    case UnApp(UnApp(UnApp(`op`, arg1), arg2), arg3) => Some((arg1, arg2, arg3))
    case _ => None
  }

  def unapply(e: Expr) = e match {
    case App(App(App(`op`, arg1), arg2), arg3) => Some((arg1, arg2, arg3))
    case _ => None
  }

  def apply(arg1: Expr, arg2: Expr, arg3: Expr): Expr = {
    App(App(App(op, arg1), arg2), arg3)
  }

  def apply(arg1: Pat, arg2: Pat, arg3: Pat): Pat = {
    UnApp(UnApp(UnApp(op, arg1), arg2), arg3)
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