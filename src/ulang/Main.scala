package ulang

import java.io.File
import java.io.FileReader

import ulang.syntax._
import ulang.command._

object Main {
  val handlers = Map(
    "definitions" -> Definitions,
    "lemmas" -> Lemmas)

  def main(args: Array[String]) {
    import syntax.Parser._

    val cmds = load(new File("test.txt"))

    for (Cmd(name, exprs) <- cmds if handlers contains name) {
      val res = handlers(name)(exprs)
      println(name)
      res foreach println
      println()
    }
  }
}