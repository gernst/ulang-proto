package ulang

import java.io.File
import java.io.FileReader
import ulang.syntax._

object Main {
  def main(args: Array[String]) {
    import Parser._

    val res = load(new File("test.txt"))

    for(cmd <- res) {
      println(cmd)
    }
  }
}