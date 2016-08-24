package ulang

package object core {
  val Wildcard = Id("_")

  val True = Tag("True")
  val False = Tag("False")

  object Pair extends Binary(",")

  val Empty = Tag("[]")
  object Cons extends Binary("::")

  type Val = Any
  type Env = Map[String, Val]

  def isTag(str: String) = {
    str.head.isUpper || Operators.constrs(str)
  }

  object Env {
    val empty: Env = Map.empty
    val default: Env = Map("=" -> builtin_equal _)
  }

  def test(b: Boolean) = if (b) True else False

  def builtin_equal(obj1: Val)(obj2: Val): Val = test(equal(obj1, obj2))

  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Id(name1), Id(name2)) =>
      name1 == name2
    case (Obj(data1, arg1), Obj(data2, arg2)) =>
      equal(data1, data2) && equal(arg1, arg2)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}