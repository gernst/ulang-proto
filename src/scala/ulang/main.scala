package ulang

import java.io.File
import java.io.FileReader

import scala.language.postfixOps

import arse._

object Main {
  def main(args: Array[String]) {
    import parser._
    shell.exec(Imports(List("compiler")))
  }
}