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

  object print extends Prim("print", 1, {
    case List(obj) =>
      println(obj); obj
  })

  object equal extends Prim("=", 2, {
    case List(obj1, obj2) =>
      reify.boolean(test(obj1, obj2))
  })

  def test(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Lit(any1), Lit(any2)) =>
      any1 == any2
    case (Tag(name1), Tag(name2)) =>
      name1 == name2
    case (Obj(data1, arg1), Obj(data2, arg2)) =>
      return test(data1, data2) && test(arg1, arg2)
    case (_: Eq, _: Eq) =>
      false
    case _ =>
      ulang.error("cannot compare " + obj1 + " and " + obj2)
  }
}
