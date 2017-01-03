package ulang

import java.io.File
import java.io.Reader

import arse._
import scala.io.StdIn

case class State(mods: Set[String], defs: List[Def]) extends Pretty {
  def +(ctx: String) = {
    State(mods + ctx, defs)
  }
  
  def ++(that: List[Def]) = {
    State(mods, defs ++ that)
  }
}

case class Model(dyn: Env) extends Pretty

object State {
  def empty = State(Set(), List())
}

object shell {
  import parser._
  import interpreter._

  val lex = Env.empty
  var st = State.empty

  def commands: Map[String, () => Any] = Map(
    ":clear" -> cmd(st = State.empty),
    ":state" -> cmd(out(st)),
    ":model" -> cmd(out(model(merged(st)))),
    ":check" -> cmd(check()))

  def prompt: String = "u> "

  def input(): String = input(prompt)
  def input(p: String): String = StdIn.readLine(p)

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def err(obj: Any) {
    Console.err.println(obj)
    Console.err.flush
  }

  def cmd(c: => Any) = { () => c }

  def main(args: Array[String]) {
    load("base")
    load("stream")
    // repl()
  }

  def compatible(pat1: List[Expr], pat2: List[Expr]) = {
    val u = new unify

    { u.unify(pat1, pat2); false } or { true }
  }

  def check() {
    merged(st).defs.collect {
      case Def(fun, Bind(cases)) =>
        cases.tails.foreach {
          case Case(pat1, _) :: xs =>
            for (Case(pat2, _) <- xs) {
              if (!compatible(pat1, pat2))
                err("patterns " + App(fun, pat1) + " and " + App(fun, pat2) + " overlap")
            }
          case Nil =>
        }
    }
  }

  def repl() {
    while (true) {
      try {
        input() match {
          case null =>
            out(":quit")
            return
          case ":quit" =>
            return
          case "" =>
          // 
          case line if commands contains line =>
            commands(line)()
          case line if line startsWith ":" =>
            sys.error("unknown command " + line)
          case line =>
            read("", line)
        }
      } catch {
        case e: StackOverflowError =>
          err("fatal: stack overflow")
        case e: Throwable =>
          err("fatal: " + e.getMessage)
          e.printStackTrace()
      }
    }
  }

  def load(name: String) {
    if (!(st.mods contains name)) {
      st += name
      read(name, new File("src/ulang/" + name + ".u"))
    }
  }

  def read(ctx: String, reader: Reader): Unit = {
    var in = tokenize(reader)

    while (!in.isEmpty) {
      val (cmd, rest) = grammar.cmd(in)
      if (rest.length == in.length)
        sys.error("syntax error at " + rest.mkString(" "))
      exec(ctx, cmd)
      in = rest
    }
  }

  def merged(st: State) = st match {
    case State(mods, defs) =>
      val funs = defs.distinct.collect {
        case Def(App(id: Id, args), rhs) if !args.isEmpty =>
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

      State(mods, merged.toList ++ consts)
  }

  def model(st: State) = st match {
    case State(_, defs) =>

      val dyn = defs.foldLeft(Env.default) {
        case (dyn, df) =>
          dyn + eval(df, lex, dyn)
      }

      Model(dyn)
  }

  def exec(ctx: String, cmd: Cmd): Unit = cmd match {
    case Imports(names) =>
      import parser._

      for (name <- names) {
        load(name)
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
      // out("checking " + existing.length + " existing patterns against " + defs.length + " new ones from " + ctx)
      st.defs.collect {
        case Def(App(fun: Id, pat1), _) =>
          defs.collect {
            case Def(App(`fun`, pat2), _) =>
              // out("checking " + App(fun, pat1) + " and " + App(fun, pat2))
              if (!compatible(pat1, pat2))
                err("patterns " + App(fun, pat1) + " and " + App(fun, pat2) + " overlap")
          }
      }

      st ++= defs

    case Tests(tests) =>
      val Model(dyn) = model(merged(st))

      new tst.Test {
        test(ctx) {
          for (Def(lhs, rhs) <- tests) {
            eval(lhs, lex, dyn) expect eval(rhs, lex, dyn)
          }
        }
      }

    case Evals(exprs) =>
      val Model(dyn) = model(merged(st))

      for (expr <- exprs) {
        out(expr + " == " + eval(expr, lex, dyn) + ";")
      }
  }
}