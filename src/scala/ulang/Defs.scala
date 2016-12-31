package ulang

import arse._

object Defs {
  import Recognizer._
  import Parser._
  import Eval._

  def compile(defs: List[Def]) = {
    val lex = Env.empty
    var dyn = Env.empty
    
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
      val fun = eval(rhs, lex, dyn)
      dyn += (name -> fun)
    }
    
    for ((name, rhs) <- consts) {
      val const = eval(rhs, lex, dyn)
      dyn += (name -> const)
    }
    
    dyn
  }
}