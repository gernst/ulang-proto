package ulang

package object calculus {
  type Rewrites = Map[String, (List[Expr], Goal) => Expr]
}