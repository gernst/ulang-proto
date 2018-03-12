package ulang

import ulang.expr.Expr
import ulang.expr.Id

package object prove {
  type Env = Map[String, Expr]

  object Env {
    val empty: Env = Map()

    def apply(dfs: List[(Id, Expr)]): Env = {
      val pairs = dfs.map { case (Id(name), rhs) => (name, rhs) }
      pairs.toMap
    }
  }
}