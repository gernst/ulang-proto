package ulang

import java.io.File
import java.io.Reader

import arse._
import arse.control._
import scala.io.StdIn

case class State(mods: Set[String], defs: List[Def]) extends Pretty {
  def +(ctx: String) = {
    State(mods + ctx, defs)
  }

  def define(that: List[Def]) = {
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

  def cmd(c: => Any) = { () => c }
  def commands: Map[String, () => Any] = Map(
    ":clear" -> cmd(st = State.empty),
    ":state" -> cmd(out(st)),
    ":model" -> cmd(out(model(merged(st)))),
    ":check" -> cmd(check()))

  val Prompt = "u> "

  def input(): String = input(Prompt)
  def input(p: String): String = StdIn.readLine(p)

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def err(obj: Any) {
    Console.err.println(obj)
    Console.err.flush
  }

  def safe[A](f: => A) = try {
    Some(f)
  } catch {
    case e: StackOverflowError =>
      err("fatal: stack overflow")
      None
    case e: Throwable =>
      err("fatal: " + e)
      // e.printStackTrace()
      None
  }

  def main(args: Array[String]) {
    safe {
      load("base")
      load("stream")
      // load("test")
      // load("regex")
    }
    // repl()
  }

  def compatible(pat1: List[Pat], pat2: List[Pat]) = {
    val u = new unify

    { u.unify(pat1, pat2); false } or { true }
  }

  def check() {
    merged(st).defs.collect {
      case Def(fun, _, Bind(cases)) =>
        cases.tails.foreach {
          case Case(pat1, _, _) :: xs =>
            for (Case(pat2, _, _) <- xs) {
              if (!compatible(pat1, pat2))
                err("patterns " + UnApp(fun, pat1) + " and " + UnApp(fun, pat2) + " overlap")
            }
          case Nil =>
        }
    }
  }

  def repl() {
    while (true) {
      safe {
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
        case Def(UnApp(id: Id, pats), cond, rhs) if !pats.isEmpty =>
          (id, Case(pats, cond, rhs))
      }

      val merged = group(funs).map {
        case (id, cases) =>
          Def(id, None, Bind(cases))
      }

      val consts = defs.collect {
        case df @ Def(_: Id, None, rhs) =>
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
      st.defs.collect {
        case Def(UnApp(fun: Id, pats1), _, _) =>
          defs.collect {
            case Def(UnApp(`fun`, pats2), _, _) =>
              if (!compatible(pats1, pats2))
                err("patterns " + UnApp(fun, pats1) + " and " + UnApp(fun, pats2) + " overlap")
          }
      }

      st = st define defs

    case Tests(tests) =>
      val Model(dyn) = model(merged(st))

      new tst.Test {
        test(ctx) {
          for (Test(phi) <- tests) {
            phi match {
              case App(Id("="), List(lhs, rhs)) =>
                eval(lhs, lex, dyn) expect eval(rhs, lex, dyn)
              case _ =>
                eval(phi, lex, dyn) expect builtin.True
            }
          }
        }
      }

    case Evals(exprs) =>
      val Model(dyn) = model(merged(st))

      for (expr <- exprs) {
        out(expr + "\n  == " + eval(expr, lex, dyn) + ";")
      }
  }
}