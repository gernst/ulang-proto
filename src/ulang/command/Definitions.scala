package ulang.command

object Definitions {
  /*def apply(exprs: List[Expr]) = {
    ???
    
    def defs = exprs collect {
      case Def(syntax.Applys(Id(name), args), rhs) =>
        (name, Lambdas(args, rhs))
    }

    val funs = defs.map(_._1).distinct
    val grouped = group(defs)

    val merged = funs.map {
      name =>
        (name, Merge.merges(grouped(name)))
    }

    var lex = Env.empty
    var dyn = Env.empty

    for ((name, rhs) <- merged) {
      val res = Eval.eval(rhs, lex, dyn)
      dyn += (name -> res)
    }

    dyn
    
  }*/
}