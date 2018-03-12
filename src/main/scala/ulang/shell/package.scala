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
import scala.collection.mutable

package object shell {
  var modules = Set.empty[String]
  var defs: List[Def] = Nil
  var inds: List[Ind] = Nil

  def model = {
    import ulang.expr.Env
    Env(exec.merge(defs), Env.empty)
  }

  def rewrites = {
    import ulang.prove.Env
    Env(exec.merge(defs))
  }

  def cmd(c: => Any) = { () => c }

  def commands: Map[String, () => Any] = Map()

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

  def read(ctx: String, text: String) {
    object whitespace extends Whitespace("\\s*")
    val in = arse.input(text)(whitespace)

    while (!in.isEmpty) {
      val cmd = grammar.cmd parse in
      exec.exec(ctx, cmd)
    }
  }
}