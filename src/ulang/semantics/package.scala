package ulang

package object semantics {
  type Val = Any
  type Env = Map[String, Val]
  type Subst = Map[String, Expr]

  object Env {
    val empty: Env = Map.empty
  }

  object Subst {
    val empty: Subst = Map.empty
  }
}