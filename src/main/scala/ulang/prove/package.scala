package ulang

import ulang.expr.Expr
import ulang.expr.Var
import ulang.expr.Pat
import ulang.expr.unify

package object prove {
  type Env = Map[Var, Expr]
  type Ind = List[(Pat, List[Goal])]

  object Env {
    val empty: Env = Map()

    def apply(dfs: List[(Var, Expr)]): Env = {
      Map(dfs: _*)
    }
    
    def free(env: Env) = {
      
    }
  }

  object Ind {
    import derive.assert

    def apply(inds: List[ulang.shell.Ind]): Ind = {
      val pairs = inds.collect {
        case ulang.shell.Ind(cases) if !cases.isEmpty =>
          val constrs = cases map Goal.normalized
          val pats = constrs.map(_.suc.toPat)
          val pat = pats reduce unify.merge
          (pat, constrs)
      }
      pairs
    }
  }
}