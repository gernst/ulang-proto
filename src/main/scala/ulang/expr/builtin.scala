package ulang.expr

import ulang.shell

object builtin {
  val Eq = Id("=")
  val Zero = Tag("0")
  val Succ = Unary(Tag("+1"))
  val True = Tag("True")
  val False = Tag("False")
  val Tuple = NAry(Tag("Tuple"))
  val Nil = Tag("Nil")
  val Cons = Binary(Tag("Cons"))

  def reify(b: Boolean) = if (b) True else False

  object print extends (List[Val] => Val) {
    override def toString = "print"

    def apply(args: List[Val]): Val = args match {
      case List(obj) =>
        println(obj)
        obj
    }
  }

  object equal extends (List[Val] => Val) {
    override def toString = "="

    def apply(args: List[Val]): Val = args match {
      case List(obj1, obj2) =>
        reify(test(obj1, obj2))
    }

    def test(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
      case (Lit(any1), Lit(any2)) =>
        any1 == any2
      case (Tag(name1), Tag(name2)) =>
        name1 == name2
      case (Obj(data1, args1), Obj(data2, args2)) =>
        if (!test(data1, data2)) false
        if (args1.length != args2.length) false
        else (args1, args2).zipped.forall((test _).tupled)
      case (_: Eq, _: Eq) =>
        false
      case _ =>
        shell.error("cannot compare " + obj1 + " and " + obj2)
    }
  }
}
