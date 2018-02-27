package ulang

import java.io.File
import java.io.Reader

import arse._
import arse.control._
import scala.io.StdIn
import scala.io.Source
import scala.runtime.NonLocalReturnControl

object shell {
  val lex = Env.empty
  var modules: Set[String] = Set()
  var defs: List[Def] = List()

  def model = Model(eval.eval(defs, lex, Env.empty))

  def cmd(c: => Any) = { () => c }
  def commands: Map[String, () => Any] = Map(
    ":clear" -> cmd({ modules = Set(); defs = List() }),
    ":model" -> cmd(out(model)))

  val Prompt = "u> "

  def main(args: Array[String]) {
    safe {
      load("mini")
      load("base")
      // load("prover")
    }
    repl()
  }

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
      e.printStackTrace()
      None
    case e: Throwable =>
      err("fatal: " + e)
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
            sys.error("unknown command " + line)
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
          sys.error("unknown notation: " + not)
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
        out(expr + "\n  = " + eval.eval(expr, lex, dyn) + ";")
      }
  }
}