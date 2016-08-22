package ulang

package object core {
  val Wildcard = Id("_")

  val True = Id("True")
  val False = Id("False")

  type Env = Map[String, Val]

  def identity[A](a: A) = a

  def isTag(str: String) = {
    str.head.isUpper || Operators.constrs(str)
  }

  object Env {
    val empty: Env = Map.empty
    val default: Env = Map("=" -> Prim(obj1 => Prim(obj2 => test(equal(obj1, obj2)))))
  }
  
  def test(b: Boolean) = if(b) True else False

  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Id(name1), Id(name2)) =>
      name1 == name2
    case (Obj(data1, arg1), Obj(data2, arg2)) =>
      equal(data1, data2) && equal(arg1, arg2)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}