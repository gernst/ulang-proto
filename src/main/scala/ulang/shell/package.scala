package ulang

import scala.io.Source
import scala.io.StdIn

import arse.Infix
import arse.Postfix
import arse.Prefix
import arse.Whitespace
import ulang.expr.App
import ulang.expr.Atom
import ulang.expr.Env
import ulang.expr.Id
import ulang.expr.Tag
import ulang.expr.builtin
import ulang.expr.eval
import ulang.expr.operators
import ulang.prove.derive

package object shell {
  val lex = Env.empty
  var modules: Set[String] = Set()
  var defs: List[Def] = List()

  def model = Model(defs, lex, Env.empty)

  def cmd(c: => Any) = { () => c }
  def commands: Map[String, () => Any] = Map(
    ":clear" -> cmd({ modules = Set(); defs = List() }),
    ":model" -> cmd(out(model)))

  val Prompt = "u> "

  def main(args: Array[String]) {
    safe {
      load("mini")
      // load("base")
      // load("prover")
    }
    // repl()
  }

  def input(): String = input(Prompt)
  def input(p: String): String = StdIn.readLine(p)

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def warning(obj: Any) = {
    Console.err.println(obj.toString)
    Console.err.flush
  }

  def error(obj: Any) = {
    sys.error(obj.toString)
  }

  def safe[A](f: => A) = try {
    Some(f)
  } catch {
    case e: StackOverflowError =>
      warning("error: stack overflow")
      e.printStackTrace()
      None
    case e: Throwable =>
      warning("error: " + e)
      e.printStackTrace()
      None
  }

  def repl() {
    var run = true
    while (run) {
      safe {
        input() match {
          case null =>
            out(":quit")
            run = false
          case ":quit" =>
            run = false
          case "" =>
          //
          case line if commands contains line =>
            commands(line)()
          case line if line startsWith ":" =>
            error("unknown command " + line)
          case line =>
            read("", line)
        }
      }
    }
  }

  def load(name: String) {
    if (!(modules contains name)) {
      modules += name
      val source = Source.fromFile("src/main/ulang/" + name + ".u")
      read(name, source.mkString)
    }
  }

  def read(ctx: String, test: String): Unit = {
    object whitespace extends Whitespace("\\s*")
    val in = arse.input(test)(whitespace)

    while (!in.isEmpty) {
      val cmd = grammar.cmd parse in
      exec(ctx, cmd)
    }
  }

  def exec(ctx: String, cmd: Cmd): Unit = cmd match {
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
      val Model(dyn) = model

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
      val Model(dyn) = model

      for (expr <- exprs) {
        val res = eval.eval(expr, lex, dyn)
        out(expr + "\n  = " + res + ";")
      }

    case Thms(props) =>
      val pairs = Model.merge(defs) map {
        case Def(Id(name), None, rhs) => (name, rhs)
      }
      val dyn = pairs.toMap

      for (Thm(goal, rule) <- props) {
        val res = derive.derive(goal, rule, dyn)
        out(res)
      }
  }
}