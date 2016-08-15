package ulang

import java.io.File
import java.io.FileReader

import ulang.syntax._
import ulang.command._

object Main {
  val handlers = Map(
    "definitions" -> Definitions)

  def main(args: Array[String]) {
    import syntax.Parser._

    val cmds = load(new File("small.txt"))

    for (Cmd(name, exprs) <- cmds if handlers contains name) {
      val res = handlers(name)(exprs)
      println(name)
      res foreach println
      println()
    }
  }
}