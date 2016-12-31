package ulang

import java.io.File
import java.io.FileReader

import scala.language.postfixOps

import arse._

object Main {
  def main(args: Array[String]) {
    import Parser._
    import Recognizer._

    val parts = "definitions" ~ grammar.module ~ "end"

    val in = parser.tokenize(new File("src/ulang/small.txt"))
    val (ps, out) = parts(in);
    val res = interpreter.eval(ps, Env.empty, Env.empty)

    for ((name, value) <- res) {
      println(name + " == " + value)
    }
  }
}