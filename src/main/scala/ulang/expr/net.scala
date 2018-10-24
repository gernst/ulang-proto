package ulang.expr

case class Net(ops: Map[Tag, Net], app: Option[Net], bound: Option[Net], check: Map[Int, Net], rhs: List[Expr]) {
  def insert(pats: List[Pat], expr: Expr): Net = pats match {
    case Nil =>
      copy(rhs = rhs :+ expr)

    case (op: Tag) :: rest =>
      val sub = ops.getOrElse(op, Net.empty)
      copy(ops = ops + (op -> sub.insert(rest, expr)))

    case UnApp(fun, arg) :: rest =>
      val sub = app.getOrElse(Net.empty)
      copy(app = Some(sub.insert(fun :: arg :: rest, expr)))

    case (fv: Free) :: rest =>
      val sub = bound.getOrElse(Net.empty)
      copy(bound = Some(sub.insert(rest, expr)))
      
    case Bound(index) :: rest =>
      val sub = check.getOrElse(index, Net.empty)
      copy( check = check + (index -> sub.insert(rest, expr)))
  }
}

object Net {
  val empty = Net(Map.empty, None, None, Map.empty, Nil)
}