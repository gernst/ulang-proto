package ulang

import ulang.command.Cmd

case class State(cmds: Map[String,List[Expr]]) {
}

object State {
  def apply(cmds: List[Cmd]): State = {
    val pairs = cmds.groupBy(_.name).map {
      case (name, cmds) =>
        (name, cmds.flatMap(_.exprs))
    }
    
    State(pairs)
  }
}