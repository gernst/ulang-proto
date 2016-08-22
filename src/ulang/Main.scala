package ulang

import java.io.File
import java.io.FileReader
import ulang.core.ULang
import ulang.core.Data

object Main {
  def main(args: Array[String]) {
    import Language._
    
    val file = new File("parser.txt")
    val source = tokenize(file)
    
    val empty = ULang(Nil)
    val (some, rest) = empty.parser(source)
    // println(some)
    
    println("rest: " + rest)
    
    val dyn = some.compile
    
    for((name, value) <- dyn if value.isInstanceOf[Data]) {
      println(name + " == " + value)
    }

    /*
    val cmds = load(new File("parser.txt"))

    for (Cmd(name, exprs) <- cmds if handlers contains name) {
      val res = handlers(name)(exprs)
      println(name)
      
      val res_sorted = res.toList.sortBy(_._1)
      for((name, value) <- res_sorted)
        println(name + " == " + value)
      println()*/
  }
}