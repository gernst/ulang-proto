package ulang.expr

object reify {
  def boolean(b: Boolean) = {
    if (b) builtin.True
    else builtin.False
  }

  def option(e: Option[Expr]) = e match {
    case None => builtin.None
    case Some(e) => builtin.Some(e)
  }

  def tuple(ps: List[Pat]) = ps match {
    case List(p) => p
    case _ => builtin.Tuple(ps)
  }

  def tuple(es: List[Expr]) = es match {
    case List(e) => e
    case _ => builtin.Tuple(es)
  }

  def list(ps: List[Pat]) = builtin.Cons(ps, builtin.Nil)
  def list(es: List[Expr]) = builtin.Cons(es, builtin.Nil)

  def list_with_tail(ps: List[Pat], pt: Pat) = builtin.Cons(ps, pt)
  def list_with_tail(es: List[Expr], et: Expr) = builtin.Cons(es, et)

  def atom(atom: Id): Expr = atom match {
    case Tag(name) =>
      App(Tag("Tag"), Lit(name))
    case Var(name) =>
      App(Tag("Id"), Lit(name))
  }

  def reify(cs: Case): Expr = cs match {
    case Case(pats, body) =>
      Apps(Tag("Case"), List(list(pats map reify), reify(body)))
  }

  def reify(pat: Pat): Expr = pat match {
    case Wildcard =>
      Tag("_")
    case a: Id =>
      atom(a)
    case Named(pat, name) =>
      Apps(Tag("Named"), List(reify(pat), Lit(name)))
    case Cond(pat, expr) =>
      Apps(Tag("Cond"), List(reify(pat), reify(expr)))
    case UnApp(fun, arg) =>
      Apps(Tag("App"), List(reify(fun), reify(arg)))
  }

  def reify(expr: Expr): Expr = expr match {
    case l: Lit =>
      l
    case id: Id =>
      atom(id)
    case App(fun, arg) =>
      Apps(Tag("App"), List(reify(fun), reify(arg)))
    case Lambda(cases) =>
      Apps(Tag("Bind"), List(list(cases map reify)))
  }

}