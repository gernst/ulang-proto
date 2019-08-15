package ulang.shell

import ulang.expr._

object check {
  def disjoint(defs: List[Def]) = {
    defs collect {
      case Def(fun, Lambda(cases)) =>
        cases.tails.foreach {
          case Case(pats1, _) :: xs =>
            for (Case(pats2, _) <- xs) {
              if (unify.test(pats1, pats2))
                ulang.warning("patterns " + UnApps(fun, pats1) + " and " + UnApps(fun, pats2) + " overlap")
            }
          case Nil =>
        }
    }
  }

  def disjoint(defs: List[Def], add: List[Def]) = {
    defs.collect {
      case Def(UnApps(fun: Var, pats1), _) =>
        add.collect {
          case Def(UnApps(`fun`, pats2), _) =>
            if (unify.test(pats1, pats2))
              ulang.warning("patterns " + UnApps(fun, pats1) + " and " + UnApps(fun, pats2) + " overlap")
        }
    }
  }
}