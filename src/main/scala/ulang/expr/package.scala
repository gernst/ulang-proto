package ulang

package object expr {
  type Stack = List[Expr]
  type Env = Map[String, Expr]
}