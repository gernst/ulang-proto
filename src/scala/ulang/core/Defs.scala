package ulang.core

import arse._

object Defs extends ulang.Language {
  import Recognizer._
  import Parser._
  import Eval._

  case class Source(defs: List[Def]) extends ulang.Source {
    override def toString = defs.mkString("definitions\n", "\n", "\nend")
  }

  case class Compiled(defs: List[(String, Expr)]) extends ulang.Compiled
  case class Linked(dyn: Env) extends ulang.Linked

  val parser = "definitions" ~ Source.from(Grammar.defs) ~ "end"

  def compile(parts: List[ulang.Source]) = {
    val defs = parts.flatMap {
      case Source(defs) => defs
      case _ => Nil
    }

    val funs = defs.collect {
      case Def(Apply(Id(name), args), rhs) if !args.isEmpty =>
        (name, Case(args, rhs))
    }

    val clos = _root_.ulang.group(funs) map {
      case (name, cases) =>
        (name, Merge(cases))
    }

    val consts = defs.collect {
      case Def(Id(name), rhs) =>
        (name, rhs)
    }

<<<<<<< HEAD
    for ((name, cases) <- _root_.ulang.group(funs)) {
      val rhs = Bind(cases)
      println(name + " == " + rhs)
      val fun = eval(rhs, lex, dyn)
      dyn += (name -> fun)
    }
=======
    List(Compiled(clos.toList ++ consts))
  }
>>>>>>> 6d54299f6c9d705683eb763e9d559ddd58c3f18b

  def link(compiled: Map[ulang.Language, ulang.Compiled]): List[ulang.Linked] = {
    val Compiled(defs) = compiled(this).asInstanceOf[Compiled]
    var dyn = Env.empty
    val lex = Env.empty

    for ((name, rhs) <- defs) {
      dyn += name -> eval(rhs, lex, dyn)
    }

    List(Linked(dyn))
  }
}