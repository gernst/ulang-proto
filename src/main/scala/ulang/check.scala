package ulang

object check {
  def check(defs: List[Def]) = {
    defs collect {
      case Def(fun, _, Bind(cases)) =>
        cases.tails.foreach {
          case Case(pat1, _, _) :: xs =>
            for (Case(pat2, _, _) <- xs) {
              if (unify(pat1, pat2))
                shell.warning("patterns " + UnApp(fun, pat1) + " and " + UnApp(fun, pat2) + " overlap")
            }
          case Nil =>
        }
    }
  }

  def check(defs: List[Def], add: List[Def]) = {
    defs.collect {
      case Def(UnApp(fun: Id, pats1), _, _) =>
        add.collect {
          case Def(UnApp(`fun`, pats2), _, _) =>
            if (unify(pats1, pats2))
              shell.warning("patterns " + UnApp(fun, pats1) + " and " + UnApp(fun, pats2) + " overlap")
        }
    }
  }
}