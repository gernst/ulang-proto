package ulang.shell

import arse.Infix
import arse.Postfix
import arse.Prefix
import ulang.expr.App
import ulang.expr.Atom
import ulang.expr.Case
import ulang.expr.Expr
import ulang.expr.Id
import ulang.expr.Lambda
import ulang.expr.Tag
import ulang.expr.UnApp
import ulang.expr.builtin
import ulang.expr.eval
import ulang.expr.operators
import ulang.prove.derive

object exec {
  import shell.defs
  import shell.inds

  def model = {
    import ulang.expr.Env
    Env(merge(defs), Env.empty)
  }

  def rewrites = {
    import ulang.prove.Env
    Env(merge(defs))
  }

  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

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

  def exec(ctx: String, cmd: Cmd) = cmd match {
    case Imports(names) =>
      for (name <- names) {
        shell.load(name)
      }

    case Notations(nots) =>
      nots foreach {
        case Fix(Prefix(prec), names) =>
          for (name <- names) { operators.prefix_ops += (Atom(name) -> prec) }
        case Fix(Postfix(prec), names) =>
          for (name <- names) { operators.postfix_ops += (Atom(name) -> prec) }
        case Fix(Infix(assoc, prec), names) =>
          for (name <- names) { operators.infix_ops += (Atom(name) -> (assoc, prec)) }
        case Data(names) =>
          for (name <- names) { operators.data += Tag(name) }
        case not =>
          ulang.error("unknown notation: " + not)
      }

    case Defs(add) =>
      check.disjoint(defs, add)
      defs ++= add

    case Tests(tests) =>
      val dyn = model

      new tst.Test {
        test(ctx) {
          for (Test(phi) <- tests) {
            phi match {
              case App(Id("="), List(lhs, rhs)) =>
                eval.eval(lhs, dyn) expect eval.eval(rhs, dyn)
              case _ =>
                eval.eval(phi, dyn) expect builtin.True
            }
          }
        }
      }

    case Evals(exprs) =>
      val dyn = model

      for (expr <- exprs) {
        val res = eval.eval(expr, dyn)
        ulang.out(expr + "\n  = " + res + ";")
      }

    case Thms(props) =>
      val dyn = rewrites

      for (Thm(goal, rule) <- props) {
        val res = derive.derive(goal, rule, dyn)
        ulang.out(res)
      }

    case Inds(add) =>
      inds ++= add
  }
}