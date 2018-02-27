package ulang.shell

import ulang.group
import ulang.expr._
import ulang.Pretty

case class Model(dyn: Env) extends Pretty

object Model {
  def apply(dfs: List[Def], lex: Env, dyn: Env): Model = {
    val funs = dfs.distinct.collect {
      case Def(UnApp(id: Id, pats), cond, rhs) if !pats.isEmpty =>
        (id, Case(pats, cond, rhs))
    }

    val merged = group(funs).map {
      case (id, cases) =>
        Def(id, None, Bind(cases))
    }

    val consts = dfs.collect {
      case df @ Def(_: Id, None, rhs) =>
        df
    }

    val all = merged.toList ++ consts
    // check.check(all)

    val newdyn = all.foldLeft(Env.default) {
      case (dyn, Def(Id(name), None, rhs)) =>
        dyn + (name -> eval.eval(rhs, lex, dyn))
    }

    Model(newdyn)
  }
}