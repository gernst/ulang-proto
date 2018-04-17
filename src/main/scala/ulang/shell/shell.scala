package ulang.shell

import scala.io.Source
import scala.io.StdIn

import arse.Whitespace

object shell {
  var modules = Set.empty[String]
  var defs: List[Def] = Nil
  var inds: List[Ind] = Nil

  def clear {
    defs = Nil
    inds = Nil
  }

  def cmd(c: => Any) = { () => c }

  def commands: Map[String, () => Any] = Map(
    ":clear" -> cmd(clear))

  val Prompt = "u> "

  def input(): String = input(Prompt)
  def input(p: String): String = StdIn.readLine(p)

  def safe[A](f: => A) = try {
    Some(f)
  } catch {
    case e: StackOverflowError =>
      ulang.warning("error: stack overflow")
      e.printStackTrace()
      None
    case e: Throwable =>
      ulang.warning("error: " + e)
      e.printStackTrace()
      None
  }

  def repl() {
    var run = true
    while (run) {
      safe {
        input() match {
          case null =>
            ulang.out(":quit")
            run = false
          case ":quit" =>
            run = false
          case "" =>
          //
          case line if commands contains line =>
            commands(line)()
          case line if line startsWith ":" =>
            ulang.error("unknown command " + line)
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