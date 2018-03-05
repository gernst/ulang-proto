package ulang

import ulang.expr.Expr

package object prove {
  type Binding = Map[String, Expr]
  
  object Binding {
    val empty: Binding = Map()
  }
}