package object ulang {
  trait Expr extends Pretty
  trait Data extends Pretty

  case class Cmd(name: String, exprs: List[Expr]) extends Pretty {
  }

  case class Id(name: String) extends Expr with Data {
    assert(!name.isEmpty)
  }
}