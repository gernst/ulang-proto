package ulang

import java.io.File
import java.io.Reader

import arse._

case class State(defs: List[Def]) {
  def ++(that: List[Def]) = State(defs ++ that)
}

object State {
  def empty = State(Nil)
}

object shell {
  import parser._
  import interpreter._

  val lex = Env.empty
  var st = State.empty

  def load(reader: Reader): Unit = {
    var in = tokenize(reader)

    while (!in.isEmpty) {
      val (cmd, rest) = grammar.cmd(in)
      if (rest.length == in.length)
        sys.error("syntax error at " + rest.mkString(" "))
      exec(cmd)
      in = rest
    }
  }

  def model(st: State) = st match {
    case State(defs) =>
      val funs = defs.collect {
        case Def(Apply(id: Id, args), rhs) if !args.isEmpty =>
          (id, Case(args, rhs))
      }

      val merged = group(funs).map {
        case (id, cases) =>
          Def(id, Bind(cases))
      }

      val consts = defs.collect {
        case df @ Def(_: Id, rhs) =>
          df
      }

      (merged ++ consts).foldLeft(Env.default) {
        case (dyn, df) =>
          dyn + eval(df, lex, dyn)
      }
  }

  def exec(cmd: Cmd): Unit = cmd match {
    case Imports(names) =>
      import parser._

      for (name <- names) {
        load(new File("src/ulang/" + name + ".txt"))
      }

    case Nots(nots) =>
      nots foreach {
        case Fix(Prefix(prec), names) =>
          for (name <- names) { operators.prefix_ops += (name -> prec) }
        case Fix(Postfix(prec), names) =>
          for (name <- names) { operators.postfix_ops += (name -> prec) }
        case Fix(Infix(assoc, prec), names) =>
          for (name <- names) { operators.infix_ops += (name -> (assoc, prec)) }
        case Data(names) =>
          operators.data ++= names
        case not =>
          sys.error("unknown notation: " + not)
      }

    case Defs(defs) =>
      st ++= defs

    case Evals(exprs) =>
      val dyn = model(st)

      for (expr <- exprs) {
        println(expr)
        print("  == ")
        print(eval(expr, lex, dyn))
        println(";")
      }
  }
}