package ulang.expr

object builtin {
  object eq extends Binary(Free("="))

  val Zero = Tag("0")
  object Succ extends Unary(Tag("+1"))

  val True = Tag("True")
  val False = Tag("False")
  object and extends Binary(Free("and"))
  object or extends Binary(Free("or"))
  object ==> extends Binary(Free("==>"))
  object <=> extends Binary(Free("<=>"))

  object Tuple extends Nary(Tag("Tuple"))

  val Nil = Tag("Nil")
  object Cons extends Binary(Tag("Cons"))

  val None = Tag("None")
  object Some extends Unary(Tag("Some"))

  object print extends Prim("print") {
    def apply(args: List[Val]): Val = args match {
      case List(obj) =>
        println(obj)
        obj
    }
  }

  object equal extends Prim("=") {
    def apply(args: List[Val]): Val = args match {
      case List(obj1, obj2) =>
        reify.boolean(test(obj1, obj2))
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
        ulang.error("cannot compare " + obj1 + " and " + obj2)
    }
  }
}
