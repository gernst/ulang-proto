package ulang

import java.io.File
import java.io.FileReader

import scala.language.postfixOps

import arse._

object Main {
  def main(args: Array[String]) {
    import parser._
    
    val mod = parse(grammar.module, new File("src/ulang/small.txt"))
    val res = interpreter.add(State.default, mod)

    for ((name, value) <- res.local) {
      println(name + " == " + value)
    }
  }
}