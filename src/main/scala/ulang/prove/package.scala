package ulang

import ulang.expr.Expr
import ulang.expr.Id
import ulang.expr.Pat
import ulang.expr.unify

package object prove {
  type Env = Map[String, Expr]
  type Ind = List[(Pat, List[Goal])]

  object Env {
    val empty: Env = Map()

    def apply(dfs: List[(Id, Expr)]): Env = {
      val pairs = dfs.map { case (Id(name), rhs) => (name, rhs) }
      pairs.toMap
    }
  }

  object Ind {
    import derive.assert
    
    def apply(inds: List[ulang.shell.Ind]): Ind = {
      val pairs = inds.collect {
        case ulang.shell.Ind(cases) if !cases.isEmpty =>
          val constrs = cases map (assert(_, Goal.empty))
          val pats = constrs.map(_.suc.toPat)
          val pat = pats reduce unify.merge
          (pat, constrs)
      }
      pairs
    }
  }
}