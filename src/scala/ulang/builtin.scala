package ulang

object builtin {
  val True = Tag("True")
  val False = Tag("False")
  def reify(b: Boolean) = if (b) True else False

  def unpair(h: Pat, t: Pat) = UnApp(Tag(","), List(h, t))
  def pair(h: Expr, t: Expr) = App(Tag(","), List(h, t))
  
  def uncons(h: Pat, t: Pat) = UnApp(Tag("::"), List(h, t))
  def cons(h: Expr, t: Expr) = App(Tag("::"), List(h, t))

  def reify_option(e: Option[Expr]) = e match {
    case None => Tag("None")
    case Some(e) => App(Tag("Some"), List(e))
  }
  
  def reify_tuple(ps: List[Pat]) = ps.reduceRight(unpair)
  def reify_tuple(es: List[Expr]) = es.reduceRight(pair)
  
  def reify_list(ps: List[Pat]) = ps.foldRight(Tag("[]"): Pat)(uncons)
  def reify_list(es: List[Expr]) = es.foldRight(Tag("[]"): Expr)(cons)

  def reify_atom(atom: Atom): Expr = atom match {
    case Tag(name) =>
      App(Tag("Tag"), List(Lit(name)))
    case Id(name) =>
      App(Tag("Id"), List(Lit(name)))
  }

  def reify(eq: LetEq): Expr = eq match {
    case LetEq(pat, arg) =>
      App(Tag("LetEq"), List(reify(pat), reify(arg)))
  }
  
  def reify(cs: Case): Expr = cs match {
    case Case(pats, cond, body) =>
      App(Tag("Case"), List(reify_list(pats map reify), reify_option(cond map reify), reify(body)))
  }

  def reify(pat: Pat): Expr = pat match {
    case Wildcard =>
      Tag("_")
    case l: Lit =>
      l
    case atom: Atom =>
      reify_atom(atom)
    case SubPat(name, pat) =>
      App(Tag("SubPat"), List(Lit(name), reify(pat)))
    case Force(pat) =>
      App(Tag("Force"), List(reify(pat)))
    case UnApp(fun, args) =>
      App(Tag("App"), List(reify(fun), reify_list(args map reify)))
  }

  def reify(expr: Expr): Expr = expr match {
    case l: Lit =>
      l
    case atom: Atom =>
      reify_atom(atom)
    case Susp(pat) =>
      App(Tag("Susp"), List(reify(pat)))
    case App(fun, args) =>
      App(Tag("App"), List(reify(fun), reify_list(args map reify)))
    case Bind(cases) =>
      App(Tag("Bind"), List(reify_list(cases map reify)))
    case IfThenElse(test, iftrue, iffalse) =>
      App(Tag("IfThenElse"), List(reify(test), reify(iftrue), reify(iffalse)))
    case LetIn(eqs, body) =>
      App(Tag("LetIn"), List(reify_list(eqs map reify), reify(body)))
    case MatchWith(args, cases) =>
      App(Tag("MatchWith"), List(reify_list(args map reify), reify_list(cases map reify)))
  }

  val equal = Prim("=", { case List(obj1, obj2) => reify(_equal(obj1, obj2)) })
  val print = Prim("print", { case List(obj) => println(obj); obj })

  def _equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Lit(any1), Lit(any2)) =>
      any1 == any2
    case (Tag(name1), Tag(name2)) =>
      name1 == name2
    case (Obj(data1, args1), Obj(data2, args2)) =>
      if (!_equal(data1, data2)) false
      if (args1.length != args2.length) false
      else (args1, args2).zipped.forall((_equal _).tupled)
    case (_: Eq, _: Eq) =>
      false
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}
