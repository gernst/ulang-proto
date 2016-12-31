package ulang

import java.io.File
import java.io.FileReader

import scala.language.postfixOps

import arse._

object Main {
  def main(args: Array[String]) {
    import Parser._
    import Recognizer._

    val parts = "definitions" ~ Grammar.defs ~ "end"

    val in = tokenize(new File("src/ulang/small.txt"))
    val (ps, out) = parts(in);
    val res = Defs.compile(ps)

    for ((name, value) <- res) {
      println(name + " == " + value)
    }
  }
}