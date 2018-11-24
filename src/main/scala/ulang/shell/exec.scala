package ulang.shell

import arse.Infix
import arse.Postfix
import arse.Prefix
import ulang.expr.App
import ulang.expr.Id
import ulang.expr.Case
import ulang.expr.Expr
import ulang.expr.Var
import ulang.expr.Lambda
import ulang.expr.Tag
import ulang.expr.UnApp
import ulang.expr.builtin
import ulang.expr.eval
import ulang.expr.operators
import ulang.prove.derive
import ulang.expr.UnApps
import ulang.expr.UnApps
import ulang.expr.Env
import ulang.expr.Pat
import ulang.expr.Lambdas

object exec {
  import shell.defs
  import shell.inds

  def model = {
    import ulang.expr.Env
    Env(merge(defs), Env.empty)
  }

  def rewrites = {
    import ulang.prove.Env
    Env(merge(defs, inds))
  }

  def inductions = {
    import ulang.prove.Ind
    Ind(inds)
  }

  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  def merge(dfs: List[Def], inds: List[Ind]): List[(Var, Expr)] = {
    merge(dfs ++ define(inds))
  }

  def merge(dfs: List[Def]): List[(Var, Expr)] = {
    println("merging")
    val bnd = dfs map {
      case Def(UnApps(id: Var, pats), body) =>
        (id, Lambdas(pats, body))
    }

    bnd map println
    /* val funs = dfs.distinct.collect {
      case Def(UnApps(id: Var, pats), rhs) if !pats.isEmpty =>
        println(id + pats.mkString(" ", " ", " = ") + rhs)
        val cs = Lambda.binding(pats, rhs)
        println(id + " = " + cs)
        (id, cs)
    }

    val merged = group(funs).map {
      case (id, lambdas) =>
        (id, lambdas reduce (_ | _))
    }

    val consts = dfs.collect {
      case Def(id: Var, rhs) =>
        println(id + " = " + rhs)
        (id, rhs)
    }

    merged.toList ++ consts
    */
    ???
  }

  def define(inds: List[Ind]): List[Def] = {
    import builtin.==>

    inds.flatMap {
      case Ind(cases) =>
        cases.collect {
          case ant ==> suc =>
            Def(suc.toPat, ant)
          case suc =>
            Def(suc.toPat, builtin.True)
        }
    }
  }

  def exec(ctx: String, cmd: Cmd) = cmd match {
    case Imports(names) =>
      for (name <- names) {
        shell.load(name)
      }

    case Notations(nots) =>
      nots foreach {
        case Fix(Prefix(prec), names) =>
          for (name <- names) { operators.prefix_ops += (Id(name) -> prec) }
        case Fix(Postfix(prec), names) =>
          for (name <- names) { operators.postfix_ops += (Id(name) -> prec) }
        case Fix(Infix(assoc, prec), names) =>
          for (name <- names) { operators.infix_ops += (Id(name) -> (assoc, prec)) }
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
              case builtin.eq(lhs, rhs) =>
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
        ulang.out(expr)
        val res = eval.eval(expr, dyn)
        ulang.out("  = " + res + ";")
      }

    case Thms(props) =>
      val dyn = rewrites
      val ind = inductions

      for (Thm(goal, rule) <- props) {
        val res = derive.derive(goal, rule, dyn, ind)
        ulang.out(res)
      }

    case add: Ind =>
      inds :+= add
  }
}