package ulang

import java.io.File
import java.io.Reader

import arse._
import scala.io.StdIn

case class State(defs: List[Def]) extends Pretty {
  def ++(that: List[Def]) = State(defs ++ that)
}

case class Model(dyn: Env) extends Pretty

object State {
  def empty = State(Nil)
}

object shell {
  import parser._
  import interpreter._

  val lex = Env.empty
  var st = State.empty

  def commands: Map[String, () => Any] = Map(
    ":state" -> cmd(println(st)),
    ":model" -> cmd(println(model(st))))

  def prompt: String = "u> "

  def input(): String = input(prompt)
  def input(p: String): String = StdIn.readLine(p)

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def err(text: String) {
    Console.err.println(text)
    Console.err.flush
  }

  def cmd(c: => Any) = { () => c }

  def main(args: Array[String]) {
    repl()
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
            read(line)
        }
      } catch {
        case e: StackOverflowError =>
          err("fatal: stack overflow")
        case e: Throwable =>
          err("fatal: " + e.getMessage)
        // e.printStackTrace()
      }
    }
  }

  def read(reader: Reader): Unit = {
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

      val dyn = (merged ++ consts).foldLeft(Env.default) {
        case (dyn, df) =>
          dyn + eval(df, lex, dyn)
      }
      
      Model(dyn)
  }

  def exec(cmd: Cmd): Unit = cmd match {
    case Imports(names) =>
      import parser._

      for (name <- names) {
        read(new File("src/ulang/" + name + ".u"))
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
      val Model(dyn) = model(st)

      for (expr <- exprs) {
        println(expr)
        print("  == ")
        print(eval(expr, lex, dyn))
        println(";")
      }
  }
}