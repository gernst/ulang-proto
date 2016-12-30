package ulang

package object core {
  val Wildcard = Id("_")

  val True = Tag("True")
  val False = Tag("False")

  object Pair extends Binary(",")

  val Empty = Tag("[]")
  object Cons extends Binary("::")

  type Val = Any
  type Fun = (List[Val], Env) => Val

  type Env = Map[String, Val]

  object Env {
    val empty: Env = Map.empty
    val default: Env = Map("=" -> builtin_equal _)
  }

  def test(b: Boolean) = if (b) True else False

  def builtin_equal(objs: List[Val], dyn: Env): Val = objs match {
    case List(obj1, obj2) =>
      test(equal(obj1, obj2))
  }

  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Id(name1), Id(name2)) =>
      name1 == name2
    case (Obj(data1, args1), Obj(data2, args2)) =>
      if(!equal(data1, data2)) false
      if(args1.length != args2.length) false
      else (args1,args2).zipped.forall((equal _).tupled)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}