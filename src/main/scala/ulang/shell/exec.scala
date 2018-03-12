package ulang.shell

import ulang.expr.Id
import ulang.expr.Lambda
import ulang.expr.Case
import ulang.expr.UnApp
import ulang.expr.Expr
import arse.Postfix
import arse.Infix
import ulang.expr.builtin
import arse.Prefix
import ulang.expr.operators
import ulang.prove.derive
import ulang.expr.Atom
import ulang.expr.App
import ulang.expr.Tag
import ulang.expr.eval

object exec {
  import ulang.shell.defs
  import ulang.shell.rewrites

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
        load(name)
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
          error("unknown notation: " + not)
      }

    case Defs(add) =>
      check.check(defs, add)
      defs ++= add

    case Tests(tests) =>
      import ulang.expr.Env
      val lex = Env.empty
      val dyn = model

      new tst.Test {
        test(ctx) {
          for (Test(phi) <- tests) {
            phi match {
              case App(Id("="), List(lhs, rhs)) =>
                eval.eval(lhs, lex, dyn) expect eval.eval(rhs, lex, dyn)
              case _ =>
                eval.eval(phi, lex, dyn) expect builtin.True
            }
          }
        }
      }

    case Evals(exprs) =>
      import ulang.expr.Env
      val lex = Env.empty
      val dyn = model

      for (expr <- exprs) {
        val res = eval.eval(expr, lex, dyn)
        out(expr + "\n  = " + res + ";")
      }

    case Thms(props) =>
      import ulang.prove.Env
      val dyn = rewrites

      for (Thm(goal, rule) <- props) {
        val res = derive.derive(goal, rule, dyn)
        out(res)
      }

    case Inds(inds) =>
      ???
  }
}