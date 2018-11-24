package ulang.expr

object builtin {
  object eq extends Binary(Var("="))

  val Zero = Tag("0")
  object Succ extends Unary(Tag("+1"))

  val True = Tag("True")
  val False = Tag("False")
  object and extends Binary(Var("and"))
  object or extends Binary(Var("or"))
  object ==> extends Binary(Var("==>"))
  object <=> extends Binary(Var("<=>"))
  object IfThenElse extends Ternary(Var("if then else"))

  object Tuple extends Nary(Tag("Tuple"))

  val Nil = Tag("Nil")
  object Cons extends Binary(Tag("Cons"))

  val None = Tag("None")
  object Some extends Unary(Tag("Some"))

  /* object print extends Prim("print", 1, {
    case List(obj) =>
      println(obj); obj
  })

  object equal extends Prim("=", 2, {
    case List(obj1, obj2) =>
      reify.boolean(test(obj1, obj2))
  }) */
}
