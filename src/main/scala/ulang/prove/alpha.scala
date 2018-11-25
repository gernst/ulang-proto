package ulang.prove

import ulang.expr.Var
import ulang.expr.Pat
import ulang.expr.Named
import ulang.expr.Cond
import ulang.expr.Lit
import ulang.expr.UnApp
import ulang.expr.Wildcard
import ulang.expr.Tag

object alpha {
  def captures(x: Var, env: Env): Boolean = {
    env exists { case (id, expr) => x in expr }
  }
  
  def captures(pat: Pat, env: Env): Boolean = pat match {
    case Wildcard | _: Lit | _: Tag =>
      false
    case x: Var =>
      captures(x, env)
    case UnApp(fun, arg) =>
      captures(fun, env) || captures(arg, env)
    case Named(pat, x) =>
      captures(pat, env) || captures(x, env)
    case Cond(pat, cond) =>
      captures(pat, env)
  }
}