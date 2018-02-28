package ulang

package object expr {
  type Val = Any

  type Env = Map[String, Val]
  type Subst = Map[Pat, Pat]

  object Subst {
    val empty: Subst = Map()
  }
}