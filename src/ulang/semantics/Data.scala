package ulang.semantics

import ulang._

case class Apply(data: Data, arg: Val) extends Data

object Applys extends ((Data, List[Val]) => Val) {
  def apply(fun: Data, args: List[Val]): Data = {
    args.foldLeft(fun)(Apply)
  }

  def unapply(expr: Data): Option[(Data, List[Val])] = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Data, args: List[Val]): (Data, List[Val]) = expr match {
    case Apply(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }
}
