package ulang.shell

import ulang.group
import ulang.expr._
import ulang.Pretty

case class Model(dyn: Env) extends Pretty

object Model {
  def merge(dfs: List[Def]): List[(Id, Expr)] = {
    val funs = dfs.distinct.collect {
      case Def(UnApp(id: Id, pats), cond, rhs) if !pats.isEmpty =>
        (id, Case(pats, cond, rhs))
    }

    val merged = group(funs).map {
      case (id, cases) =>
        (id, Lambda(cases))
    }

    val consts = dfs.collect {
      case Def(id: Id, None, rhs) =>
        (id, rhs)
    }

    merged.toList ++ consts
  }

  def apply(dfs: List[Def], lex: Env, dyn: Env): Model = {
    val all = merge(dfs)
    // check.check(all)

    val newdyn = all.foldLeft(Env.default) {
      case (dyn, (Id(name), rhs)) =>
        dyn + (name -> eval.eval(rhs, lex, dyn))
    }

    Model(newdyn)
  }
}