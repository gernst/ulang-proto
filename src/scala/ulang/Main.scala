package ulang

import java.io.File
import java.io.FileReader

import scala.language.postfixOps

import arse._

import ulang.core.Defs
import ulang.core.Import
import ulang.grammar.Grammar

object Main {
  def main(args: Array[String]) {
    import Language._
    import Parser._
    
    val languages = List(Import, Defs, Grammar)
    val part = parse((in: List[String]) => alt(languages.map(_.parser), in))
    val parts = part *
    
    /*
    val state = new State(List(new Import(), new Definitions(), new Grammar()))
    */
    val in = tokenize(new File("src/ulang/small.txt"))
    val (ps, out) = parts(in);
    val res = languages.map(_.build(ps))
    
    /*
    println(state)
    
    state.languages map (_.build())
    
    val file = new File("grammar.txt")
    val source = tokenize(file)

    val empty = Grammar(Nil)
    val (some, rest) = empty.parser(source)
    println(some)

    println("rest: " + rest)

    val dyn = some.compile

    for ((name, value) <- dyn if value.isInstanceOf[Data]) {
      println(name + " == " + value)
    }
    */

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