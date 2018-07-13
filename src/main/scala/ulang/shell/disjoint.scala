package ulang.shell

import ulang.expr._

object check {
  def disjoint(defs: List[Def]) = {
    defs collect {
      case Def(fun, Lambda(cases)) =>
        cases.tails.foreach {
          case Case(pat1, _) :: xs =>
            for (Case(pat2, _) <- xs) {
              if (unify.test(pat1, pat2))
                ulang.warning("patterns " + UnApp(fun, pat1) + " and " + UnApp(fun, pat2) + " overlap")
            }
          case Nil =>
        }
    }
  }

  def disjoint(defs: List[Def], add: List[Def]) = {
    defs.collect {
      case Def(UnApps(fun: Free, pats1), _) =>
        add.collect {
          case Def(UnApps(`fun`, pats2), _) =>
            if (unify.test(pats1, pats2))
              ulang.warning("patterns " + UnApps(fun, pats1) + " and " + UnApps(fun, pats2) + " overlap")
        }
    }
  }
}