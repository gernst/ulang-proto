package ulang.core

import arse._

import ulang._

case class Defs(defs: List[Def]) extends Source {
  override def toString = defs.mkString("definitions\n", "\n", "\nend")
}

case class Model(dyn: Env) extends Compiled {
  override def toString = dyn.keys.mkString("Model(", ", ", ")")
}

object Defs extends (List[Def] => Defs) with Language {
  import Recognizer._
  import Parser._
  import Eval._

  val parser = "definitions" ~ Defs.from(Grammar.defs) ~ "end"

  def build(parts: List[Source]) = {
    var dyn = Env.default
    val lex = Env.empty
    
    val defs = parts.flatMap {
      case Defs(defs) => defs
      case _ => Nil
    }

    val funs = defs.collect {
      case Def(Apply(Id(name), args), rhs) if !args.isEmpty =>
        (name, Case(args, rhs))
    }

    val consts = defs.collect {
      case Def(Id(name), rhs) =>
        (name, rhs)
    }

    for ((name, cases) <- _root_.ulang.group(funs)) {
      val rhs = Bind(cases)
      println(name + " == " + rhs)
      val fun = eval(rhs, lex, dyn)
      dyn += (name -> fun)
    }

    for ((name, rhs) <- consts) {
      val const = eval(rhs, lex, dyn)
      println(name + " == " + const)
      dyn += (name -> const)
    }
    
    Model(dyn)
  }
}