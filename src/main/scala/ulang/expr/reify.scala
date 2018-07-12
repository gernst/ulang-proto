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

  def atom(atom: Atom): Expr = atom match {
    case Lit(value) =>
      App(Tag("Lit"), List(atom))
    case Bound(index) =>
      App(Tag("Bound"), List(Lit(index)))
    case Tag(name) =>
      App(Tag("Tag"), List(Lit(name)))
    case Free(name) =>
      App(Tag("Id"), List(Lit(name)))
  }

  def reify(cs: Case): Expr = cs match {
    case Case(pats, cond, body) =>
      App(Tag("Case"), List(list(pats map reify), option(cond map reify), reify(body)))
  }

  def reify(pat: Pat): Expr = pat match {
    case Wildcard =>
      Tag("_")
    case l: Lit =>
      l
    case a: Atom =>
      atom(a)
    case SubPat(name, pat) =>
      App(Tag("SubPat"), List(Lit(name), reify(pat)))
    case UnApp(fun, args) =>
      App(Tag("App"), List(reify(fun), list(args map reify)))
  }

  def reify(expr: Expr): Expr = expr match {
    case l: Lit =>
      l
    case a: Atom =>
      atom(a)
    case App(fun, args) =>
      App(Tag("App"), List(reify(fun), list(args map reify)))
    case Lambda(cases) =>
      App(Tag("Bind"), List(list(cases map reify)))
    case IfThenElse(test, iftrue, iffalse) =>
      App(Tag("IfThenElse"), List(reify(test), reify(iftrue), reify(iffalse)))
    case MatchWith(args, cases) =>
      App(Tag("MatchWith"), List(list(args map reify), list(cases map reify)))
  }

}