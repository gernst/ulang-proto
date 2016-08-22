package ulang.core

import arse._

case class ULang(defs: List[Def]) extends _root_.ulang.Language {
  import Recognizer._
  import Parser._
  import Eval._
  import Merge._

  def extend(add: List[Def]) = ULang(defs ++ add)
  val parser = (extend _).from("definitions" ~ Grammar.defs)

  override def toString = defs.mkString("definitions\n", "\n", "\n")

  def compile = {
    val lex = Env.empty
    var dyn = Env.default

    val funs = defs.collect {
      case Def(Applys(Id(name), args), rhs) if !args.isEmpty =>
        (name, Lambdas(args, rhs))
    }

    val consts = defs.collect {
      case Def(Id(name), rhs) =>
        (name, rhs)
    }

    for ((name, cases) <- _root_.ulang.group(funs)) {
      val rhs = Merge(cases)
      // println(name + " == " + rhs)
      val fun = eval(rhs, lex, dyn)
      dyn += (name -> fun)
    }

    for ((name, rhs) <- consts) {
      // println(name + " == " + rhs)
      val const = eval(rhs, lex, dyn)
      dyn += (name -> const)
    }

    dyn
  }
}